package com.github.shk0da.goldendragon.utils;

import ru.tinkoff.piapi.contract.v1.Quotation;

public class IndicatorsUtil {

    public static double toDouble(Quotation quotation) {
        return toDouble(quotation.getUnits(), quotation.getNano());
    }

    private static double toDouble(long units, int nano) {
        return units + Double.parseDouble("0." + nano);
    }
}
