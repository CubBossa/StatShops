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
	public void saveShop() {

	}

	@Override
	public void deleteShop() {

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
	public void saveEntry() {

	}

	@Override
	public Discount createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags) {
		return null;
	}

	@Override
	public Map<UUID, Discount> loadDiscounts() {
		return new HashMap<>();
	}

	@Override
	public void saveDiscount(Discount discount) {

	}
}
