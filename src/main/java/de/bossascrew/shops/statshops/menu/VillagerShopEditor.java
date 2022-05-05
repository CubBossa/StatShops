package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import de.cubbossa.menuframework.inventory.implementations.RectInventoryMenu;

public class VillagerShopEditor extends RectInventoryMenu {

	private final VillagerShop shop;

	public VillagerShopEditor(VillagerShop shop) {
		super(Message.VILLAGER_SHOP_TITLE, 3);
		this.shop = shop;
	}
}
