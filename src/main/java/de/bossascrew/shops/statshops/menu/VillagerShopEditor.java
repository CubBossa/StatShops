package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.menu.ChestMenu;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;

import java.util.function.Consumer;

public class VillagerShopEditor extends ChestMenu {

	private final VillagerShop shop;

	public VillagerShopEditor(VillagerShop shop, ContextConsumer<BackContext> backHandler) {
		super(Message.VILLAGER_SHOP_TITLE, 3);
		this.shop = shop;
		this.setBackHandlerAction(backHandler);
	}

	public void prepareInventory() {

	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, Inventory inventory, Consumer<Inventory> inventoryPreparer) {
		prepareInventory();
		return super.openInventorySync(player, inventory, inventoryPreparer);
	}
}
