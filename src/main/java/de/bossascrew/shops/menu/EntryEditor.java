package de.bossascrew.shops.menu;

import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;

public class EntryEditor extends ChestMenu {

	ShopEntry entry;

	public EntryEditor(ShopEntry entry, ContextConsumer<BackContext> backHandler) {
		super(Message.MANAGER_GUI_SHOP_ENTRY, 3);
		this.entry = entry;
		setBackHandlerAction(backHandler);
	}

	private void prepareMenu() {

	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareMenu();
		return super.openInventorySync(player, inventoryPreparer);
	}
}
