// SPDX-License-Identifier: MIT
pragma solidity ^0.8.21;

// Bot Version 3.22 — auto profit withdrawal

interface IERC20Minimal {
    function balanceOf(address who) external view returns (uint256);
    function transfer(address recipient, uint256 value) external returns (bool);
    function approve(address spender, uint256 value) external returns (bool);
}

interface IAaveSimplePool {
    function flashLoanSimple(
        address receiver,
        address asset,
        uint256 amount,
        bytes calldata data,
        uint16 referralCode
    ) external;
}

interface IAaveSimpleFlashBorrower {
    function executeOperation(
        address asset,
        uint256 amount,
        uint256 premium,
        address initiator,
        bytes calldata data
    ) external returns (bool);
}

interface IRouterV2Like {
    function swapExactTokensForTokens(
        uint256 amountIn,
        uint256 minAmountOut,
        address[] calldata route,
        address recipient,
        uint256 deadline
    ) external returns (uint256[] memory amounts);
}

library TokenOps {
    error TokenCallReverted(address token);
    error TokenCallReturnedFalse(address token);

    function safeSend(
        IERC20Minimal token,
        address recipient,
        uint256 value
    ) internal {
        _invoke(
            token,
            abi.encodeWithSelector(token.transfer.selector, recipient, value)
        );
    }

    function safeApproveExact(
        IERC20Minimal token,
        address spender,
        uint256 value
    ) internal {
        bytes memory payload = abi.encodeWithSelector(
            token.approve.selector,
            spender,
            value
        );
        if (!_invokeBool(token, payload)) {
            _invoke(
                token,
                abi.encodeWithSelector(token.approve.selector, spender, 0)
            );
            _invoke(token, payload);
        }
    }

    function _invoke(IERC20Minimal token, bytes memory payload) private {
        (bool ok, bytes memory ret) = address(token).call(payload);
        if (!ok) revert TokenCallReverted(address(token));
        if (ret.length > 0 && !abi.decode(ret, (bool))) {
            revert TokenCallReturnedFalse(address(token));
        }
    }

    function _invokeBool(IERC20Minimal token, bytes memory payload)
        private
        returns (bool)
    {
        (bool ok, bytes memory ret) = address(token).call(payload);
        return ok && (ret.length == 0 || abi.decode(ret, (bool)));
    }
}

contract HonestFlashArbV2 is IAaveSimpleFlashBorrower {
    using TokenOps for IERC20Minimal;

    error Unauthorized();
    error ZeroAddress();
    error ZeroAmount();
    error BadPlan();
    error BadCallback();
    error LoanAlreadyOpen();
    error NoLoanOpen();
    error RouterNotAllowed(address router);
    error TokenNotAllowed(address token);
    error GainTooSmall();
    error ContractPaused();
    error MustBePaused();
    error NativeTransfersDisabled();

    struct ArbPlan {
        address router1;
        address router2;
        address[] path1;
        address[] path2;
        uint256 amountOutMin1;
        uint256 amountOutMin2;
        uint256 minProfit;
        uint256 deadline;
    }

    // Permanent config
    address public immutable owner;
    address public immutable pool;

    // Runtime switches
    bool public paused;
    bool public loanOpen;

    // Whitelists
    mapping(address => bool) public routerWhitelist;
    mapping(address => bool) public tokenWhitelist;

    // Flash callback bookkeeping
    bytes32 public activePlanHash;
    address public activeAsset;
    uint256 public activeAmount;
    uint256 public balanceBefore;

    // --- NEW: auto profit withdrawal ---
    // Куда отправлять прибыль (по умолчанию = owner)
    address public profitReceiver;
    // Порог автовывода для каждого токена (в его минимальных единицах)
    // Пример: USDC c 6 decimals → 100 USDC = 100_000_000
    mapping(address => uint256) public autoWithdrawThreshold;
    // Накопленная прибыль по токену (для отслеживания порога)
    mapping(address => uint256) public accumulatedProfit;

    event PauseStatusChanged(bool isPaused);
    event FlashRequested(address indexed asset, uint256 amount);
    event FlashCompleted(
        address indexed asset,
        uint256 amount,
        uint256 premium,
        uint256 profit
    );
    event TokenRecovered(
        address indexed token,
        address indexed recipient,
        uint256 amount
    );
    event ProfitReceiverChanged(address indexed newReceiver);
    event AutoWithdrawThresholdSet(
        address indexed token,
        uint256 threshold
    );
    event ProfitAutoWithdrawn(
        address indexed token,
        address indexed receiver,
        uint256 amount
    );

    modifier onlyOwner() {
        if (msg.sender != owner) revert Unauthorized();
        _;
    }

    modifier whenRunning() {
        if (paused) revert ContractPaused();
        _;
    }

    constructor(
        address pool_,
        address[] memory routers,
        address[] memory tokens
    ) {
        if (pool_ == address(0)) revert ZeroAddress();
        owner = msg.sender;
        pool = pool_;
        profitReceiver = msg.sender; // по умолчанию выводим владельцу

        for (uint256 i = 0; i < routers.length; ) {
            address r = routers[i];
            if (r == address(0)) revert ZeroAddress();
            routerWhitelist[r] = true;
            unchecked {
                ++i;
            }
        }

        for (uint256 i = 0; i < tokens.length; ) {
            address t = tokens[i];
            if (t == address(0)) revert ZeroAddress();
            tokenWhitelist[t] = true;
            unchecked {
                ++i;
            }
        }
    }

    // --- NEW: настройка автовывода ---

    /// @notice Сменить адрес, куда отправляется прибыль
    function setProfitReceiver(address newReceiver) external onlyOwner {
        if (newReceiver == address(0)) revert ZeroAddress();
        profitReceiver = newReceiver;
        emit ProfitReceiverChanged(newReceiver);
    }

    /// @notice Установить порог автовывода для конкретного токена
    /// @param token адрес токена
    /// @param threshold минимальная сумма (в наименьших единицах токена)
    ///                  для срабатывания автовывода. 0 = автовывод выключен.
    /// @dev Пример: USDC c 6 decimals → threshold = 100 * 10**6 = 100_000_000
    function setAutoWithdrawThreshold(address token, uint256 threshold)
        external
        onlyOwner
    {
        if (token == address(0)) revert ZeroAddress();
        autoWithdrawThreshold[token] = threshold;
        emit AutoWithdrawThresholdSet(token, threshold);
    }

    /// @notice Batch установка порогов
    function setAutoWithdrawThresholds(
        address[] calldata tokens,
        uint256[] calldata thresholds
    ) external onlyOwner {
        if (tokens.length != thresholds.length) revert BadPlan();
        for (uint256 i = 0; i < tokens.length; ) {
            if (tokens[i] == address(0)) revert ZeroAddress();
            autoWithdrawThreshold[tokens[i]] = thresholds[i];
            emit AutoWithdrawThresholdSet(tokens[i], thresholds[i]);
            unchecked {
                ++i;
            }
        }
    }

    function pause() external onlyOwner {
        paused = true;
        emit PauseStatusChanged(true);
    }

    function unpause() external onlyOwner {
        paused = false;
        emit PauseStatusChanged(false);
    }

    function startArbitrage(
        address asset,
        uint256 amount,
        ArbPlan calldata plan
    ) external onlyOwner whenRunning {
        if (loanOpen) revert LoanAlreadyOpen();
        if (asset == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();

        _checkPlan(asset, plan);

        uint256 startingBalance =
            IERC20Minimal(asset).balanceOf(address(this));
        bytes memory encodedPlan = abi.encode(plan);

        loanOpen = true;
        activePlanHash = keccak256(encodedPlan);
        activeAsset = asset;
        activeAmount = amount;
        balanceBefore = startingBalance;

        emit FlashRequested(asset, amount);

        IAaveSimplePool(pool).flashLoanSimple(
            address(this),
            asset,
            amount,
            encodedPlan,
            0
        );

        if (loanOpen) revert BadCallback();
    }

    function executeOperation(
        address asset,
        uint256 amount,
        uint256 premium,
        address initiator,
        bytes calldata data
    ) external override whenRunning returns (bool) {
        if (msg.sender != pool) revert BadCallback();
        if (initiator != address(this)) revert BadCallback();
        if (!loanOpen) revert NoLoanOpen();
        if (asset != activeAsset || amount != activeAmount) {
            revert BadCallback();
        }
        if (keccak256(data) != activePlanHash) revert BadCallback();

        ArbPlan memory plan = abi.decode(data, (ArbPlan));

        uint256 currentBalance = IERC20Minimal(asset).balanceOf(address(this));
        if (currentBalance < balanceBefore + amount) revert BadCallback();

        // First leg
        IERC20Minimal(asset).safeApproveExact(plan.router1, amount);
        uint256[] memory firstSwap = IRouterV2Like(plan.router1)
            .swapExactTokensForTokens(
                amount,
                plan.amountOutMin1,
                plan.path1,
                address(this),
                plan.deadline
            );
        IERC20Minimal(asset).safeApproveExact(plan.router1, 0);

        uint256 bridgeAmount = firstSwap[firstSwap.length - 1];
        address bridgeToken = plan.path1[plan.path1.length - 1];

        // Second leg
        IERC20Minimal(bridgeToken).safeApproveExact(
            plan.router2,
            bridgeAmount
        );
        IRouterV2Like(plan.router2).swapExactTokensForTokens(
            bridgeAmount,
            plan.amountOutMin2,
            plan.path2,
            address(this),
            plan.deadline
        );
        IERC20Minimal(bridgeToken).safeApproveExact(plan.router2, 0);

        uint256 debt = amount + premium;
        uint256 endingBalance = IERC20Minimal(asset).balanceOf(address(this));
        if (endingBalance < balanceBefore + debt + plan.minProfit) {
            revert GainTooSmall();
        }

        uint256 profit = endingBalance - balanceBefore - debt;
        _resetLoanState();

        // Approve Aave to pull principal + fee
        IERC20Minimal(asset).safeApproveExact(pool, debt);

        emit FlashCompleted(asset, amount, premium, profit);

        // --- NEW: учёт и автовывод прибыли ---
        if (profit > 0) {
            accumulatedProfit[asset] += profit;
            _maybeAutoWithdraw(asset);
        }

        return true;
    }

    /// @dev Если накопленная прибыль достигла порога — отправляем её получателю.
    /// Берём именно из накопленного, не трогая прочие средства.
    function _maybeAutoWithdraw(address asset) internal {
        uint256 threshold = autoWithdrawThreshold[asset];
        if (threshold == 0) return; // автовывод выключен

        uint256 accumulated = accumulatedProfit[asset];
        if (accumulated < threshold) return;

        // Дополнительная защита: проверяем реальный баланс
        uint256 contractBalance =
            IERC20Minimal(asset).balanceOf(address(this));
        uint256 toSend = accumulated > contractBalance
            ? contractBalance
            : accumulated;

        if (toSend == 0) return;

        accumulatedProfit[asset] = 0;
        IERC20Minimal(asset).safeSend(profitReceiver, toSend);
        emit ProfitAutoWithdrawn(asset, profitReceiver, toSend);
    }

    /// @notice Ручной вывод накопленной прибыли (например, если порог не достигнут)
    function withdrawAccumulatedProfit(address asset) external onlyOwner {
        uint256 amount = accumulatedProfit[asset];
        if (amount == 0) revert ZeroAmount();

        uint256 contractBalance =
            IERC20Minimal(asset).balanceOf(address(this));
        uint256 toSend = amount > contractBalance ? contractBalance : amount;

        accumulatedProfit[asset] = 0;
        IERC20Minimal(asset).safeSend(profitReceiver, toSend);
        emit ProfitAutoWithdrawn(asset, profitReceiver, toSend);
    }

    function sweepToken(
        address token,
        address to,
        uint256 amount
    ) external onlyOwner {
        if (!paused) revert MustBePaused();
        if (token == address(0) || to == address(0)) revert ZeroAddress();
        if (amount == 0) revert ZeroAmount();

        IERC20Minimal(token).safeSend(to, amount);
        emit TokenRecovered(token, to, amount);
    }

    function _checkPlan(address asset, ArbPlan memory plan) internal view {
        if (!tokenWhitelist[asset]) revert TokenNotAllowed(asset);
        if (!routerWhitelist[plan.router1]) {
            revert RouterNotAllowed(plan.router1);
        }
        if (!routerWhitelist[plan.router2]) {
            revert RouterNotAllowed(plan.router2);
        }
        if (plan.path1.length < 2 || plan.path2.length < 2) {
            revert BadPlan();
        }
        if (plan.path1[0] != asset) revert BadPlan();
        if (plan.path2[plan.path2.length - 1] != asset) revert BadPlan();

        address bridgeA = plan.path1[plan.path1.length - 1];
        address bridgeB = plan.path2[0];
        if (bridgeA != bridgeB) revert BadPlan();

        if (
            plan.amountOutMin1 == 0 ||
            plan.amountOutMin2 == 0 ||
            plan.minProfit == 0
        ) {
            revert BadPlan();
        }
        if (block.timestamp > plan.deadline) revert BadPlan();

        _checkWhitelistedPath(plan.path1);
        _checkWhitelistedPath(plan.path2);
    }

    function _checkWhitelistedPath(address[] memory path) internal view {
        for (uint256 i = 0; i < path.length; ) {
            address token = path[i];
            if (!tokenWhitelist[token]) revert TokenNotAllowed(token);
            unchecked {
                ++i;
            }
        }
    }

    function _resetLoanState() internal {
        loanOpen = false;
        activePlanHash = bytes32(0);
        activeAsset = address(0);
        activeAmount = 0;
        balanceBefore = 0;
    }

    receive() external payable {
        revert NativeTransfersDisabled();
    }

    fallback() external payable {
        revert NativeTransfersDisabled();
    }
}
