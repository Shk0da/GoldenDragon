package com.github.shk0da.GoldenDragon.strategy;
import static com.github.shk0da.GoldenDragon.utils.IndicatorsUtil.toDouble;
import static java.time.OffsetDateTime.now;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.github.shk0da.GoldenDragon.config.DatabaseConfig;
import com.github.shk0da.GoldenDragon.config.MainConfig;
import com.github.shk0da.GoldenDragon.service.TCSService;

import ru.tinkoff.piapi.contract.v1.CandleInterval;
import ru.tinkoff.piapi.contract.v1.HistoricCandle;
import ru.tinkoff.piapi.contract.v1.Share;

public class CandleLoader {

    private final MainConfig mainConfig;
    private final DatabaseConfig databaseConfig;
    private final TCSService tcsService;

    public CandleLoader(MainConfig mainConfig, DatabaseConfig databaseConfig,TCSService tcsService) {
        this.mainConfig = mainConfig;
        this.databaseConfig = databaseConfig;
        this.tcsService = tcsService;
    }

    public void run() {
        try {
            var currentTime = now();
            List<Share> stocks = tcsService.getMoexShares();
            stocks.forEach(stock -> {
                for (int i = 365; i >= 0; i = i - 7) {
                    var start = currentTime.minusDays(i + 7);
                    var end = currentTime.minusDays(i);
                    List<HistoricCandle> h1candles = tcsService.getCandles(
                            stock.getFigi(),
                            start,
                            end,
                            CandleInterval.CANDLE_INTERVAL_HOUR);
                    System.out.println("Loading: " + stock.getFigi() + "[" + start + " -> " + end + "]");
                    h1candles.forEach(it -> insertCandle(it, stock.getFigi(), "H1"));
                    try {
                        TimeUnit.MILLISECONDS.sleep(100);
                    } catch (InterruptedException skip) {
                        // nothing
                    }
                }
            });
        } catch (Exception ex) {
            System.err.println(ex.getMessage());
        }
    }

    public void insertCandle(HistoricCandle candle, String name, String tf) {
        try (Connection con = DriverManager.getConnection(
            databaseConfig.getJdbcUrl(), databaseConfig.getJdbcUser(), databaseConfig.getJdbcPassword())) {
            var dateTime = new Timestamp(candle.getTime().getSeconds() * 1000);
            var open = toDouble(candle.getOpen());
            var high = toDouble(candle.getHigh());
            var low = toDouble(candle.getLow());
            var close = toDouble(candle.getClose());
            var volume = candle.getVolume();

            String sql = "INSERT INTO candle(name, date_time, open, high, low, close, volume, tf) " +
                    "VALUES (?, ?, ?, ?, ?, ?, ?, ?) " +
                    "ON CONFLICT (name, tf, date_time) DO UPDATE " +
                    "SET open = excluded.open, high = excluded.high, low = excluded.low, close = excluded.close, volume = excluded.volume;";
            PreparedStatement ps = con.prepareStatement(sql);
            ps.setString(1, name);
            ps.setTimestamp(2, dateTime);
            ps.setDouble(3, open);
            ps.setDouble(4, high);
            ps.setDouble(5, low);
            ps.setDouble(6, close);
            ps.setInt(7, (int) volume);
            ps.setString(8, tf);
            ps.executeUpdate();
        } catch (Exception ex) {
            System.out.println(ex.getMessage());
        }
    }
}
