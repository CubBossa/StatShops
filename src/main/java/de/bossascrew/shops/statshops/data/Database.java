package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.general.entry.ShopEntry;
import org.bukkit.Location;
import org.bukkit.inventory.ItemStack;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

public interface Database {

	Customer loadCustomer(UUID uuid);

	void saveCustomer(Customer customer);

	Shop createShop(String nameFormat, UUID uuid);

	Map<UUID, Shop> loadShops();

	void saveShop(Shop shop);

	void deleteShop(Shop shop);

	ShopEntry createEntry(UUID uuid, Shop shop, ItemStack displayItem, int slot);

	Map<UUID, Shop> loadEntries(Shop shop);

	void saveEntry(ShopEntry shopEntry);

	void deleteEntry(ShopEntry shopEntry);

	Discount createDiscount(String nameFormat, SortedSet<LocalDateTime> start, Duration duration, double percent, String... tags);

	Map<UUID, Discount> loadDiscounts();

	void saveDiscount(Discount discount);

	void deleteDiscount(Discount discount);

	Limit createLimit(String name);

	Map<UUID, Limit> loadLimits();

	void saveLimit(Limit limit);

	void deleteLimit(Limit limit);

	EntryTemplate createTemplate(String name);

	Map<UUID, EntryTemplate> loadTemplates();

	void saveTemplate(EntryTemplate template);

	void deleteTemplate(EntryTemplate template);

	Map<Location, UUID> loadShopBlockMapping();

	void mapShopToBlock(Shop shop, Location location);

}
