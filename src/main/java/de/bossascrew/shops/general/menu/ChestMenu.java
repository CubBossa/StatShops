package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.general.menu.contexts.ClickContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.TextUtils;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.function.Consumer;

public class ChestMenu extends RowedOpenableMenu {

	public ChestMenu(Message message, int rows) {
		this(message.getTranslation(), rows, null);
	}

	public ChestMenu(Component title, int rows) {
		this(title, rows, null);
	}

	public ChestMenu(Component title, int rows, @Nullable ContextConsumer<CloseContext> closeHandler) {
		this(title, rows, rows * ROW_SIZE - 1, null, closeHandler);
	}

	public ChestMenu(Component title, int rows, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler, @Nullable ContextConsumer<CloseContext> closeHandler) {
		this(title, InventoryType.CHEST, rows, rows * ROW_SIZE - 1, defaultClickHandler, closeHandler);
	}

	public ChestMenu(Component title, int rows, int backSlot, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler,
					 @Nullable ContextConsumer<CloseContext> closeHandler) {
		this(title, InventoryType.CHEST, rows, backSlot, defaultClickHandler, closeHandler);
	}

	public ChestMenu(Component title, InventoryType type, int rows, int backSlot, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler,
					 @Nullable ContextConsumer<CloseContext> closeHandler) {
		super(title, type, rows, backSlot, defaultClickHandler, closeHandler);
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {

		Inventory inventory = Bukkit.createInventory(null, slots.length, TextUtils.toLegacy(getTitle()));
		return openInventorySync(player, inventory, inventoryPreparer);
	}
}
