package de.bossascrew.shops.statshops.api.data;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.statshops.api.ShopEntry;

import java.util.*;

public interface Database {

	Customer loadCustomer(UUID uuid);

	void saveCustomer(Customer customer);


	Map<UUID, Shop> loadShops();

	void saveShop(Shop shop);

	void deleteShop(Shop shop);


	Map<UUID, ShopEntry> loadEntries(Shop shop);

	void saveEntry(ShopEntry shopEntry);

	void deleteEntry(ShopEntry shopEntry);


	Map<UUID, Discount> loadDiscounts();

	void saveDiscount(Discount discount);

	void deleteDiscount(Discount discount);


	Map<UUID, Limit> loadLimits();

	void saveLimit(Limit limit);

	void deleteLimit(Limit limit);


	Map<UUID, EntryTemplate> loadTemplates();

	void saveTemplate(EntryTemplate template);

	void deleteTemplate(EntryTemplate template);
}
