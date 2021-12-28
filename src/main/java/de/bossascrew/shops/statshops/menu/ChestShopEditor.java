package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;

import java.util.HashMap;
import java.util.Map;

public class ChestShopEditor {

	private final ChestMenuShop shop;
	private final Map<Integer, ChestShopPageEditor> map;
	private final ContextConsumer<BackContext> backHandler;

	@Getter
	@Setter
	private boolean freezeItems = true;

	public ChestShopEditor(ChestMenuShop shop, ContextConsumer<BackContext> backHandler) {
		this.shop = shop;
		this.map = new HashMap<>();
		this.backHandler = backHandler;
	}

	public void openInventory(Player player, int shopPage) {
		ChestShopPageEditor pageMenu = get(shopPage);
		if (pageMenu == null) {
			pageMenu = new ChestShopPageEditor(shop, shopPage, backHandler, this);
			map.put(shopPage, pageMenu);
		}
		pageMenu.openInventory(player);
	}

	public ChestShopPageEditor get(int shopPage) {
		return map.get(shopPage);
	}
}
