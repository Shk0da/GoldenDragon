package com.github.shk0da.GoldenDragon.config;

/**
 * Dividend calendar URLs for DivFlow strategy.
 * Provides data sources for dividend analysis from Smart-Lab, Dohod, and Investing.com.
 */
public class DivFlowConfig {
    public static final String SMART_LAB_DIV_CALENDAR = "https://smart-lab.ru/dividends/index/order_by_cut_off_date/desc/?is_approved=1";
    public static final String DOHOD_DIV_CALENDAR = "https://www.dohod.ru/ik/analytics/dividend";
    public static final String INVESTING_DIV_CALENDAR = "https://ru.investing.com/dividends-calendar/Service/getCalendarFilteredData";
}
