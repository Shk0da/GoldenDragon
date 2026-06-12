require('dotenv').config();

module.exports = {
    // Dry-run режим
    dryRun: process.env.DRY_RUN,

    // RPC
    rpcUrl: process.env.MAINNET_RPC_URL,

    // Кошельки
    arbOwnerPrivateKey: process.env.ARB_OWNER_PRIVATE_KEY,
    flashbotsAuthKey: process.env.FLASHBOTS_AUTH_KEY,

    // Контракт
    arbContractAddress: process.env.ARB_CONTRACT_ADDRESS,

    // Flashbots
    flashbotsRelay: 'https://relay.flashbots.net',

    // Параметры арбитража
    loanAmount: process.env.LOAN_AMOUNT_USDC,
    minProfitUsdc: process.env.MIN_PROFIT_USDC,

    // Aave V3
    aavePool: '0x87870Bca3F3fD6335C3F4ce8392D69350B4fA4E2',
    aaveFeeRate: 5, // 0.05% = 5 базисных пунктов

    // Роутеры
    routers: {
        uniswapV2: '0x7a250d5630B4cF539739dF2C5dAcb4c659F2488D',
        sushiswap: '0xd9e1cE17f2641f24aE83637ab66a2cca9C378B9F',
    },

    // Токены (mainnet)
    tokens: {
        WETH: '0xC02aaA39b223FE8D0A0e5C4F27eAD9083C756Cc2',
        USDC: '[Secrets26]',
        USDT: '0xdAC17F958D2ee523a2206206994597C13D831ec7',
        DAI: '0x6B175474E89094C44Da98b954EedeAC495271d0F',
    },

    // Decimals токенов
    decimals: {
        WETH: 18,
        USDC: 6,
        USDT: 6,
        DAI: 18,
    },

    // Пары для мониторинга арбитража
    // Каждая пара: borrow токен → bridge → обратно borrow
    pairs: [
        {
            name: 'USDC↔WETH',
            borrowSymbol: 'USDC',
            bridgeSymbol: 'WETH',
        },
        {
            name: 'USDC↔DAI',
            borrowSymbol: 'USDC',
            bridgeSymbol: 'DAI',
        },
    ],

    // Slippage tolerance (0.5%)
    slippageBps: 50,

    // Интервал опроса цен (мс)
    pollIntervalMs: 3000,

    // Лимит газа на транзакцию
    gasLimit: 800000,
};
