package de.bossascrew.shops.menu;

import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.ShopMode;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;

import java.util.HashMap;
import java.util.Map;

public class ChestShopEditor {

	private final ChestMenuShop shop;
	private final Map<ShopMode, Map<Integer, ChestShopPageEditor>> map;
	private final ContextConsumer<BackContext> backHandler;

	@Getter
	@Setter
	private boolean freezeItems = false;

	public ChestShopEditor(ChestMenuShop shop, ContextConsumer<BackContext> backHandler) {
		this.shop = shop;
		this.map = new HashMap<>();
		this.backHandler = backHandler;
	}

	public void openInventory(Player player, ShopMode mode, int shopPage) {
		ChestShopPageEditor pageMenu = get(mode, shopPage);
		if (pageMenu == null) {
			pageMenu = new ChestShopPageEditor(shop, mode, shopPage, backHandler, this);
			put(mode, shopPage, pageMenu);
		}
		pageMenu.openInventory(player);
	}

	public ChestShopPageEditor get(ShopMode shopMode, int shopPage) {
		if (map.containsKey(shopMode)) {
			map.get(shopMode).get(shopPage);
		}
		return null;
	}

	public void put(ShopMode shopMode, int shopPage, ChestShopPageEditor menu) {
		Map<Integer, ChestShopPageEditor> innerMap = map.getOrDefault(shopMode, new HashMap<>());
		innerMap.put(shopPage, menu);
		if (!map.containsKey(shopMode)) {
			map.put(shopMode, innerMap);
		}
	}
}
