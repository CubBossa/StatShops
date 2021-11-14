package de.bossascrew.shops.menu;

import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;

public class ShopEditorMenu extends RowedOpenableMenu {

	public ShopEditorMenu(Component title, int rows) {
		super(title, InventoryType.CHEST, rows, 0, null, null);
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		return null;
	}
}
