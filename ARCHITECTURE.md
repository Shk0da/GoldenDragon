# GoldenDragon Architecture

## System Overview

GoldenDragon is an automated trading bot for the Russian stock market (MOEX) that supports multiple trading strategies, money management, and ML-powered market regime detection.

### Key Features
- **Multiple Trading Strategies**: UnifiedStrategy, LevelTrader, RSX, DivFlow, Rebalance
- **Money Management**: Risk control, position sizing, kill switch, adaptive capital
- **ML Integration**: XGBoost-based regime detection and trade prediction
- **Backtesting Engine**: Historical simulation with portfolio management
- **Real-time Trading**: Live market data via Tinkoff Invest API
- **Notifications**: Telegram bot for trade alerts and system events

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        GoldenDragon                              │
├─────────────────────────────────────────────────────────────────┤
│  Entry Points                                                   │
│  - GoldenDragon (main)                                          │
│  - BacktestRunner                                               │
│  - MlModelTrainer                                               │
├─────────────────────────────────────────────────────────────────┤
│  Strategy Layer                                                 │
│  - BaseStrategy (abstract engine)                               │
│  - UnifiedStrategy, LevelTrader, RSX, DivFlow, Rebalance        │
├─────────────────────────────────────────────────────────────────┤
│  Filters                                                        │
│  - MarketRegimeFilter (trend/range detection)                   │
│  - GroupConfirmationFilter (peer validation)                    │
│  - BadWeatherFilter (market conditions)                         │
├─────────────────────────────────────────────────────────────────┤
│  Money Management                                               │
│  - RiskManager (daily limits, consecutive losses)               │
│  - KillSwitch (emergency halt)                                  │
│  - PositionSizer (size calculation)                             │
│  - StopLossManager (trailing stops)                             │
│  - AdaptiveCapital (anti-martingale)                            │
│  - PerformanceTracker (statistics)                              │
├─────────────────────────────────────────────────────────────────┤
│  ML Layer                                                       │
│  - TradeDataCollector (labeled data)                            │
│  - MlModelTrainer (XGBoost training)                            │
│  - MlPredictionService (predictions)                            │
├─────────────────────────────────────────────────────────────────┤
│  Service Layer                                                  │
│  - TCSService (Tinkoff Invest API)                              │
│  - TelegramNotifyService (notifications)                        │
│  - TradingViewService (market scanning)                         │
├─────────────────────────────────────────────────────────────────┤
│  Repository Layer                                               │
│  - TickerRepository (ticker metadata)                           │
│  - FigiRepository (FIGI mapping)                                │
│  - PricesRepository (order book cache)                          │
├─────────────────────────────────────────────────────────────────┤
│  Model Layer                                                    │
│  - Candle, Position, TradingDecision                            │
│  - TickerInfo, MarketDepthSnapshot                              │
│  - Config (ML configuration)                                    │
└─────────────────────────────────────────────────────────────────┘
```

## Component Details

### Strategy Layer
All strategies extend `BaseStrategy` which provides:
- Lifecycle management (trading hours, EOD close)
- Historical candle loading and caching
- Parallel ticker processing
- Position management
- Filter integration

Strategies implement `decide()` method to generate trading signals.

### Filter Layer
Stateless, thread-safe filters that validate signals:
- **MarketRegimeFilter**: ADX-based trend/range detection
- **GroupConfirmationFilter**: Peer instrument validation
- **BadWeatherFilter**: Market condition checks (volume, volatility, spread)

### Money Management Layer
Integrated risk control system:
- **RiskManager**: Daily loss limits, consecutive loss limits
- **KillSwitch**: Emergency trading halt on critical drawdown
- **PositionSizer**: Calculate position size based on risk
- **StopLossManager**: Dynamic stop-loss updates
- **AdaptiveCapital**: Anti-martingale position sizing
- **PerformanceTracker**: Trade statistics and drawdown tracking

### ML Layer
XGBoost-based prediction system:
- **TradeDataCollector**: Collects labeled trade data (entry/exit, indicators, outcome)
- **MlModelTrainer**: Trains models (general or per-ticker)
- **MlPredictionService**: Predicts trade probability and adjusts position size

### Service Layer
External system integrations:
- **TCSService**: Tinkoff Invest API wrapper (market data, order execution)
- **TelegramNotifyService**: Async Telegram notifications
- **TradingViewService**: Market scanner for fundamental analysis

### Repository Layer
In-memory data storage with ConcurrentHashMap:
- **TickerRepository**: Ticker metadata (FIGI, ISIN, currency, type)
- **FigiRepository**: Ticker-to-FIGI mapping
- **PricesRepository**: Current order book data

### Model Layer
Data transfer objects:
- **Market Data**: Candle, MarketDepthSnapshot, MarketTradeTick
- **Positions**: Position, PositionInfo, PortfolioPosition
- **Decisions**: TradingDecision
- **Instruments**: TickerInfo, TickerType, Market, Group

## Data Flow

```
1. GoldenDragon.main()
   └─> Initialize configs and services
   └─> Select strategy
   └─> strategy.run()

2. strategy.run()
   └─> Load tickers and candles
   └─> Compute capital allocation
   └─> Start parallel ticker processing

3. processTicker(ticker)
   └─> Load/refresh candles
   └─> Apply filters (BadWeather, MarketRegime, GroupConfirmation)
   └─> Call decide() for signal
   └─> Execute action (OPEN/CLOSE/HOLD)
   └─> Update ML data collector

4. ML Pipeline (async)
   └─> TradeDataCollector.registerTrade()
   └─> MlAutoTrainingService.tryRetrain() (periodic)
   └─> MlPredictionService.predict() (on entry)
```

## Thread Model

- **BaseStrategy**: Thread pool with one thread per ticker
- **Services**: Thread-safe (ConcurrentHashMap, ExecutorService)
- **Filters**: Stateless and thread-safe
- **Money Management**: Atomic variables for thread safety
- **Repositories**: ConcurrentHashMap-based, thread-safe

## Configuration

All configuration loaded from `application.properties`:
- API keys and account settings
- Strategy parameters (tickers, indicators, limits)
- Money management settings (risk %, drawdown limits)
- ML settings (data paths, model output)
- Backtest settings (periods, threads, balance)

## External Dependencies

- **Tinkoff Invest API**: Market data and order execution
- **Telegram Bot API**: Notifications
- **TradingView API**: Market scanning
- **TA-Lib**: Technical indicators (RSI, MACD, ATR)
- **XGBoost**: ML model training and prediction
- **Gson**: JSON serialization

## Build and Run

```bash
# Full build
./gradlew clean uberJar

# Run backtest
./gradlew clean runBacktest

# Run strategy
./gradlew runStrategy -Pstrategy=UnifiedStrategy

# Train ML model
./gradlew runMlTraining

# Full ML pipeline
./gradlew generateModel
```
