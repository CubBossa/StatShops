package de.bossascrew.shops.menu;

import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.VillagerShop;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;

import java.util.function.Consumer;

public class VillagerShopEditor extends ChestMenu {

	private final VillagerShop shop;

	public VillagerShopEditor(VillagerShop shop, ContextConsumer<BackContext> backHandler) {
		super(/*TODO*/ (Message) null, 3);
		this.shop = shop;
		this.setBackHandlerAction(backHandler);
	}

	private void prepareInventory() {

	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, Inventory inventory, Consumer<Inventory> inventoryPreparer) {
		prepareInventory();
		return super.openInventorySync(player, inventory, inventoryPreparer);
	}
}
