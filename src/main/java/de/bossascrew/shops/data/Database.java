package de.bossascrew.shops.data;

import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.entry.ShopEntry;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.UUID;

public interface Database {

	Shop createShop(String nameFormat, UUID uuid);

	Map<UUID, Shop> loadShops();

	void saveShop(Shop shop);

	void deleteShop(Shop shop);

	ShopEntry createEntry();

	Map<UUID, Shop> loadEntries(Shop shop);

	void saveEntry();

	Discount createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags);

	Map<UUID, Discount> loadDiscounts();

	void saveDiscount(Discount discount);

}
