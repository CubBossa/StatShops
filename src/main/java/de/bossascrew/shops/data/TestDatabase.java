package de.bossascrew.shops.data;

import de.bossascrew.shops.handler.ShopHandler;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.entry.ShopEntry;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class TestDatabase implements Database {

	@Override
	public Shop createShop(String nameFormat, UUID uuid) {
		Shop shop = new ChestMenuShop(nameFormat, uuid);
		shop.setDefaultShopMode(ShopHandler.getInstance().getShopModes().get(0));
		return shop;
	}

	@Override
	public Map<UUID, Shop> loadShops() {
		Map<UUID, Shop> map = new HashMap<>();
		Shop s1 = createShop("<rainbow>ExampleShop</rainbow>", UUID.randomUUID());
		Shop s2 = createShop("<white>Boring Shop", UUID.randomUUID());
		map.put(s1.getUUID(), s1);
		map.put(s2.getUUID(), s2);
		return map;
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
		Map<UUID, Discount> map = new HashMap<>();
		Discount d1 = new Discount(UUID.randomUUID(), "<red>XMas Discount", LocalDateTime.now(), Duration.of(3, ChronoUnit.DAYS), 10, null);


		map.put(d1.getUuid(), d1);
		return map;
	}

	@Override
	public void saveDiscount(Discount discount) {

	}
}
