// Минимальные ABI для работы

const ROUTER_V2_ABI = [
    'function getAmountsOut(uint amountIn, address[] memory path) view returns (uint[] memory amounts)',
];

const ARB_CONTRACT_ABI = [
    'function startArbitrage(address asset, uint256 amount, tuple(address router1, address router2, address[] path1, address[] path2, uint256 amountOutMin1, uint256 amountOutMin2, uint256 minProfit, uint256 deadline) plan) external',
    'function owner() view returns (address)',
    'function paused() view returns (bool)',
    'function accumulatedProfit(address) view returns (uint256)',
    'event FlashCompleted(address indexed asset, uint256 amount, uint256 premium, uint256 profit)',
];

module.exports = {
    ROUTER_V2_ABI,
    ARB_CONTRACT_ABI,
};
