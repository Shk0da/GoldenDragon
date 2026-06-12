const { ethers } = require('ethers');
const config = require('./config');
const { ARB_CONTRACT_ABI } = require('./abis');
const PriceMonitor = require('./priceMonitor');
const ArbFinder = require('./arbFinder');
const TxBuilder = require('./txBuilder');
const FlashbotsSender = require('./flashbotsSender');

async function main() {
    console.log('🚀 Starting arbitrage bot...\n');

    // 1. Инициализация
    const provider = new ethers.providers.JsonRpcProvider(config.rpcUrl);
    const wallet = new ethers.Wallet(config.arbOwnerPrivateKey, provider);
    const authSigner = new ethers.Wallet(config.flashbotsAuthKey);

    console.log(`👛 Arb Owner: ${wallet.address}`);
    console.log(`🎫 Flashbots Auth: ${authSigner.address}`);
    console.log(`📜 Contract: ${config.arbContractAddress}\n`);

    // 2. Проверка контракта
    const contract = new ethers.Contract(
        config.arbContractAddress,
        ARB_CONTRACT_ABI,
        provider
    );
    const owner = await contract.owner();
    const paused = await contract.paused();

    if (owner.toLowerCase() !== wallet.address.toLowerCase()) {
        throw new Error(`❌ Wallet is not owner. Contract owner: ${owner}`);
    }
    if (paused) {
        throw new Error('❌ Contract is paused');
    }
    console.log('✅ Contract checks passed\n');

    // 3. Создание модулей
    const priceMonitor = new PriceMonitor(provider, config);
    const arbFinder = new ArbFinder(priceMonitor, config);
    const txBuilder = new TxBuilder(provider, wallet, config);
    const flashbotsSender = new FlashbotsSender(provider, authSigner, config);
    await flashbotsSender.init();

    // 4. Главный цикл
    let iteration = 0;
    let attempts = 0;
    let successes = 0;

    console.log('🔍 Starting price monitoring...\n');

    while (true) {
        iteration++;
        try {
            // Проверяем все пары
            for (const pair of config.pairs) {
                const opportunity = await arbFinder.findOpportunity(pair);

                if (!opportunity) continue;

                console.log(`\n💎 [${iteration}] Found opportunity: ${pair.name} (${opportunity.direction})`);
                console.log(`   Loan: ${ethers.utils.formatUnits(opportunity.loanAmount, 6)} USDC`);
                console.log(`   Expected profit: ${ethers.utils.formatUnits(opportunity.profit, 6)} USDC`);

                attempts++;

                // Строим план
                const plan = arbFinder.buildPlan(opportunity);

                // Получаем номер блока для таргетинга
                const blockNumber = await provider.getBlockNumber();
                const targetBlock = blockNumber + 1;

                // Подписываем транзакцию
                const { signedTx } = await txBuilder.buildSignedArbTx(
                    opportunity.borrowToken,
                    opportunity.loanAmount,
                    plan,
                    targetBlock
                );

                // Симулируем
                console.log('   🧪 Simulating...');
                const sim = await flashbotsSender.simulate([signedTx], targetBlock);

                if (!sim.success) {
                    console.log(`   ❌ Simulation failed: ${sim.error}`);
                    continue;
                }

                console.log(`   ✅ Simulation OK. Gas used: ${sim.gasUsed}`);

                // После успешной симуляции
                if (config.dryRun === 'true') {
                    console.log('   🏃 DRY RUN — skipping send');
                    continue;
                }

                // Отправляем
                console.log(`   📤 Sending bundle to block ${targetBlock}...`);
                const result = await flashbotsSender.sendBundle([signedTx], targetBlock);

                if (result.success) {
                    successes++;
                    console.log(`   🎉 INCLUDED IN BLOCK ${targetBlock}!`);
                } else {
                    console.log(`   ⏭  Not included (status ${result.status})`);
                }
            }
        } catch (err) {
            console.error(`❌ Error in iteration ${iteration}:`, err.message);
        }

        if (iteration % 20 === 0) {
            console.log(`\n📊 Stats: ${iteration} iterations, ${attempts} attempts, ${successes} successes\n`);
        }

        // Пауза между итерациями
        await sleep(config.pollIntervalMs);
    }
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

// Запуск с обработкой ошибок
main().catch(err => {
    console.error('💥 Fatal error:', err);
    process.exit(1);
});

// Graceful shutdown
process.on('SIGINT', () => {
    console.log('\n👋 Shutting down...');
    process.exit(0);
});
