const { ethers } = require('ethers');
const { ROUTER_V2_ABI } = require('./abis');

class PriceMonitor {
    constructor(provider, config) {
        this.provider = provider;
        this.config = config;

        // Создаём контракты роутеров
        this.uniswap = new ethers.Contract(
            config.routers.uniswapV2,
            ROUTER_V2_ABI,
            provider
        );
        this.sushiswap = new ethers.Contract(
            config.routers.sushiswap,
            ROUTER_V2_ABI,
            provider
        );
    }

    /**
     * Получить выходную сумму свопа на конкретном роутере
     * @param {Contract} router - контракт роутера
     * @param {BigNumber} amountIn - входная сумма
     * @param {string[]} path - путь свопа (адреса токенов)
     * @returns {BigNumber} - выходная сумма
     */
    async getAmountOut(router, amountIn, path) {
        try {
            const amounts = await router.getAmountsOut(amountIn, path);
            return amounts[amounts.length - 1];
        } catch (err) {
            // Пары может не существовать
            return ethers.BigNumber.from(0);
        }
    }

    /**
     * Получить цены на обоих DEX одновременно
     */
    async getPrices(amountIn, path) {
        const [uniOut, sushiOut] = await Promise.all([
            this.getAmountOut(this.uniswap, amountIn, path),
            this.getAmountOut(this.sushiswap, amountIn, path),
        ]);
        return { uniOut, sushiOut };
    }
}

module.exports = PriceMonitor;
