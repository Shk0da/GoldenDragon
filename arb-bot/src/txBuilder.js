const { ethers } = require('ethers');
const { ARB_CONTRACT_ABI } = require('./abis');

class TxBuilder {
    constructor(provider, wallet, config) {
        this.provider = provider;
        this.wallet = wallet;
        this.config = config;

        this.contract = new ethers.Contract(
            config.arbContractAddress,
            ARB_CONTRACT_ABI,
            wallet
        );
    }

    /**
     * Получить текущий nonce
     */
    async getNonce() {
        return await this.provider.getTransactionCount(this.wallet.address, 'pending');
    }

    /**
     * Получить актуальные параметры газа (EIP-1559)
     */
    async getGasParams() {
        const block = await this.provider.getBlock('latest');
        const baseFee = block.baseFeePerGas;

        // Priority fee: 2 gwei (адекватный tip)
        const maxPriorityFeePerGas = ethers.utils.parseUnits('2', 'gwei');
        // Max fee: base * 2 + priority (защита от скачков)
        const maxFeePerGas = baseFee.mul(2).add(maxPriorityFeePerGas);

        return { maxFeePerGas, maxPriorityFeePerGas };
    }

    /**
     * Построить и подписать транзакцию startArbitrage
     */
    async buildSignedArbTx(asset, amount, plan, targetBlock) {
        const txData = await this.contract.populateTransaction.startArbitrage(
            asset,
            amount,
            plan
        );

        const { maxFeePerGas, maxPriorityFeePerGas } = await this.getGasParams();
        const nonce = await this.getNonce();

        const tx = {
            ...txData,
            chainId: 1,
            type: 2, // EIP-1559
            maxFeePerGas,
            maxPriorityFeePerGas,
            gasLimit: this.config.gasLimit,
            nonce,
        };

        const signedTx = await this.wallet.signTransaction(tx);
        return { signedTx, tx };
    }
}

module.exports = TxBuilder;
