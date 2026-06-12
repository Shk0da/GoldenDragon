const { ethers } = require('ethers');

class ArbFinder {
    constructor(priceMonitor, config) {
        this.priceMonitor = priceMonitor;
        this.config = config;
    }

    /**
     * Применить slippage tolerance к amountOutMin
     */
    applySlippage(amount, slippageBps) {
        return amount.mul(10000 - slippageBps).div(10000);
    }

    /**
     * Рассчитать долг по flash loan (amount + Aave fee 0.05%)
     */
    calculateDebt(loanAmount) {
        const fee = loanAmount.mul(this.config.aaveFeeRate).div(10000);
        return loanAmount.add(fee);
    }

    /**
     * Проверить арбитражную возможность для одной пары
     * @param {Object} pair - конфиг пары
     * @returns {Object|null} - план арбитража или null
     */
    async findOpportunity(pair) {
        const borrowToken = this.config.tokens[pair.borrowSymbol];
        const bridgeToken = this.config.tokens[pair.bridgeSymbol];
        const loanAmount = ethers.BigNumber.from(this.config.loanAmount);

        // Путь 1: borrow → bridge
        const path1 = [borrowToken, bridgeToken];
        // Путь 2: bridge → borrow
        const path2 = [bridgeToken, borrowToken];

        // Получаем цены на первой ноге (borrow → bridge) на обоих DEX
        const firstLegPrices = await this.priceMonitor.getPrices(
            loanAmount,
            path1
        );

        // Проверяем оба направления:
        // Вариант A: купить bridge на Uniswap, продать на Sushiswap
        // Вариант B: купить bridge на Sushiswap, продать на Uniswap
        const opportunities = [];

        if (firstLegPrices.uniOut.gt(0)) {
            // bridge получили на Uniswap, теперь смотрим обратный своп на Sushi
            const backOut = await this.priceMonitor.getAmountOut(
                this.priceMonitor.sushiswap,
                firstLegPrices.uniOut,
                path2
            );
            if (backOut.gt(0)) {
                opportunities.push({
                    direction: 'UNI→SUSHI',
                    router1: this.config.routers.uniswapV2,
                    router2: this.config.routers.sushiswap,
                    midAmount: firstLegPrices.uniOut,
                    finalAmount: backOut,
                });
            }
        }

        if (firstLegPrices.sushiOut.gt(0)) {
            const backOut = await this.priceMonitor.getAmountOut(
                this.priceMonitor.uniswap,
                firstLegPrices.sushiOut,
                path2
            );
            if (backOut.gt(0)) {
                opportunities.push({
                    direction: 'SUSHI→UNI',
                    router1: this.config.routers.sushiswap,
                    router2: this.config.routers.uniswapV2,
                    midAmount: firstLegPrices.sushiOut,
                    finalAmount: backOut,
                });
            }
        }

        // Считаем прибыль для каждой возможности
        const debt = this.calculateDebt(loanAmount);
        const minProfit = ethers.BigNumber.from(this.config.minProfitUsdc);

        let best = null;
        for (const opp of opportunities) {
            if (opp.finalAmount.lte(debt)) continue; // убыток
            const profit = opp.finalAmount.sub(debt);
            if (profit.lt(minProfit)) continue; // ниже порога

            if (!best || profit.gt(best.profit)) {
                best = {
                    ...opp,
                    pair,
                    borrowToken,
                    bridgeToken,
                    loanAmount,
                    debt,
                    profit,
                    path1,
                    path2,
                };
            }
        }

        return best;
    }

    /**
     * Построить ArbPlan для отправки в контракт
     */
    buildPlan(opportunity) {
        const slippage = this.config.slippageBps;

        return {
            router1: opportunity.router1,
            router2: opportunity.router2,
            path1: opportunity.path1,
            path2: opportunity.path2,
            amountOutMin1: this.applySlippage(opportunity.midAmount, slippage),
            amountOutMin2: this.applySlippage(opportunity.finalAmount, slippage),
            minProfit: ethers.BigNumber.from(this.config.minProfitUsdc),
            deadline: Math.floor(Date.now() / 1000) + 120, // +2 минуты
        };
    }
}

module.exports = ArbFinder;
