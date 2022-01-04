package de.bossascrew.shops.statshops.api.data;

import java.util.Map;

/**
 * Provides default pricing where the price is mapped to a key.
 * This can be either a namespaced key like {@code <file>:<cfg-section>:<material>}
 * or simply the material name. The latter might be overwritten by another {@link DefaultPricingDatabase}
 */
public interface DefaultPricingDatabase {

	Map<String, Double> loadPricing(String databaseKey);
}
