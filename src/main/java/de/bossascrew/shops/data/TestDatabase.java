package de.bossascrew.shops.data;

import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.entry.ShopEntry;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class TestDatabase implements Database {

	@Override
	public Shop createShop(String nameFormat, UUID uuid) {
		return new ChestMenuShop(nameFormat, uuid);
	}

	@Override
	public Map<UUID, Shop> loadShops() {
		return new HashMap<>();
	}

	@Override
	public void saveShop(Shop shop) {

	}

	@Override
	public void deleteShop(Shop shop) {

	}

	@Override
	public ShopEntry createEntry() {
		return null;
	}

	@Override
	public Map<UUID, Shop> loadEntries(Shop shop) {
		return new HashMap<>();
	}

	@Override
	public void saveEntry(ShopEntry shopEntry) {

	}

	@Override
	public void deleteEntry(ShopEntry shopEntry) {

	}

	@Override
	public Discount createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags) {
		return new Discount(UUID.randomUUID(), nameFormat, start, duration, percent, null, tags);
	}

	@Override
	public Map<UUID, Discount> loadDiscounts() {
		return new HashMap<>();
	}

	@Override
	public void saveDiscount(Discount discount) {

	}
}
