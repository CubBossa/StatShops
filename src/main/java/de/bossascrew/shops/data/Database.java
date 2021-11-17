package de.bossascrew.shops.data;

import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Limit;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.ShopEntry;
import org.bukkit.inventory.ItemStack;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

public interface Database {

	Shop createShop(String nameFormat, UUID uuid);

	Map<UUID, Shop> loadShops();

	void saveShop(Shop shop);

	void deleteShop(Shop shop);

	ShopEntry createEntry(UUID uuid, Shop shop, ItemStack displayItem, ShopMode shopMode, int slot);

	Map<UUID, Shop> loadEntries(Shop shop);

	void saveEntry(ShopEntry shopEntry);

	void deleteEntry(ShopEntry shopEntry);

	Discount createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags);

	Map<UUID, Discount> loadDiscounts();

	void saveDiscount(Discount discount);

	void deleteDiscount(Discount discount);

	Limit createLimit(int limit);

	Map<UUID, Limit> loadLimits();

	void saveLimit(Limit limit);

	void deleteLimit(Limit limit);
}
