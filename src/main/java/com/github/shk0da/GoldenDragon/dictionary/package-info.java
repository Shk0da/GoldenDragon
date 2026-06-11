/**
 * Dictionaries and reference data for GoldenDragon application.
 *
 * <h2>Package Purpose</h2>
 * <p>The {@code dictionary} package contains reference data for currency conversion,
 * ticker mapping, and other static data. Dictionaries are used by services
 * and strategies for data normalization.</p>
 *
 * <h2>Key Classes</h2>
 * <ul>
 *   <li>{@link com.github.shk0da.GoldenDragon.dictionary.CurrenciesDictionary} — currency dictionary.
 *       Provides methods for:
 *       <ul>
 *         <li>Currency name conversion (RUB, USD, EUR → full names).</li>
 *         <li>Getting currency by instrument ticker.</li>
 *         <li>Checking supported currencies.</li>
 *       </ul>
 *       <br><b>Usage</b>: {@code TCSService} for currency pair handling.</li>
 * </ul>
 *
 * <h2>Extension</h2>
 * <p>New dictionaries can be added if needed:</p>
 * <ul>
 *   <li>Economic sector dictionary (for ticker grouping).</li>
 *   <li>Issuer country dictionary.</li>
 *   <li>Trading venue dictionary.</li>
 * </ul>
 *
 * <h2>Thread Safety</h2>
 * <p>Dictionaries are immutable and thread-safe. Data is loaded at startup
 * and not modified at runtime.</p>
 *
 * @see com.github.shk0da.GoldenDragon.service.TCSService
 * @see com.github.shk0da.GoldenDragon.model.TickerInfo
 */
package com.github.shk0da.GoldenDragon.dictionary;
