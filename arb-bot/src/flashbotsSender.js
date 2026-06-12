const { FlashbotsBundleProvider } = require('@flashbots/ethers-provider-bundle');

class FlashbotsSender {
    constructor(provider, authSigner, config) {
        this.provider = provider;
        this.authSigner = authSigner;
        this.config = config;
        this.flashbotsProvider = null;
    }

    async init() {
        this.flashbotsProvider = await FlashbotsBundleProvider.create(
            this.provider,
            this.authSigner,
            this.config.flashbotsRelay,
            'mainnet'
        );
        console.log('✅ Flashbots provider initialized');
    }

    /**
     * Симуляция bundle для проверки прибыльности
     */
    async simulate(signedTxs, targetBlock) {
        try {
            const simulation = await this.flashbotsProvider.simulate(
                signedTxs,
                targetBlock
            );

            if ('error' in simulation) {
                return { success: false, error: simulation.error.message };
            }

            // Проверяем, что все транзакции прошли
            for (const result of simulation.results) {
                if ('error' in result) {
                    return {
                        success: false,
                        error: `Tx revert: ${result.error} ${result.revert || ''}`,
                    };
                }
            }

            return {
                success: true,
                gasUsed: simulation.totalGasUsed,
                coinbaseDiff: simulation.coinbaseDiff,
            };
        } catch (err) {
            return { success: false, error: err.message };
        }
    }

    /**
     * Отправка bundle
     */
    async sendBundle(signedTxs, targetBlock) {
        const bundleResponse = await this.flashbotsProvider.sendRawBundle(
            signedTxs,
            targetBlock
        );

        if ('error' in bundleResponse) {
            return { success: false, error: bundleResponse.error.message };
        }

        // Ждём результат
        const waitResponse = await bundleResponse.wait();

        // 0 = включено в блок, 1 = блок прошёл, 2 = nonce too high
        return {
            success: waitResponse === 0,
            status: waitResponse,
            bundleHash: bundleResponse.bundleHash,
        };
    }
}

module.exports = FlashbotsSender;
