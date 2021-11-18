package de.bossascrew.shops.menu;

import com.google.common.base.Preconditions;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.InventoryHandler;
import de.bossascrew.shops.menu.contexts.ClickContext;
import de.bossascrew.shops.menu.contexts.CloseContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

public class BottomTopChestMenu extends ChestMenu {

	public static int INDEX_DIFFERENCE = 100;

	@Getter
	private final int bottomRows;
	@Getter
	private final int[] slots;

	private final Map<Player, ItemStack[]> inventories;

	public BottomTopChestMenu(Component title, int rows, int bottomRows) {
		super(title, rows);
		this.bottomRows = bottomRows;
		this.slots = getSlotsFromRows(rows, bottomRows);
		this.inventories = new HashMap<>();
		setCloseHandler(null);
	}

	public void saveInventory(Player player) {
		ItemStack[] inv = new ItemStack[bottomRows * ROW_SIZE];
		for (int i = 0; i < bottomRows * ROW_SIZE; i++) {
			inv[i] = player.getInventory().getItem(i + ROW_SIZE);
		}
		this.inventories.put(player, inv);
	}

	public void loadInventory(Player player) {
		if (!inventories.containsKey(player)) {
			ShopPlugin.getInstance().log(LoggingPolicy.WARN, "Could not restore inventory because no entry exists for " + player.getName());
			return;
		}
		for (int i = 0; i < bottomRows * ROW_SIZE; i++) {
			player.getInventory().setItem(i + ROW_SIZE, inventories.get(player)[i]);
		}
	}

	@Override
	public boolean handleAction(Player player, InventoryClickEvent event) {

		if (event.getClickedInventory() == null) {
			return false;
		}
		Inventory clickedInventory = event.getClickedInventory();
		int clickedSlot = event.getClickedInventory().getType().equals(InventoryType.PLAYER) ? event.getSlot() + INDEX_DIFFERENCE : event.getSlot();
		if (Arrays.stream(slots).noneMatch(value -> value == clickedSlot)) {
			return false;
		}
		ItemStack clickedItem = getItemStack(clickedSlot);
		ClickContext context = new ClickContext(player, clickedItem, clickedSlot, event.getClick(), defaultCancel.getOrDefault(event.getClick(), true));

		ContextConsumer<ClickContext> clickHandler = getOrDefaultHandler(clickedSlot, event.getClick());
		if (clickHandler != null) {
			//Handlung ausführen und Exceptions abfangen
			try {
				clickHandler.accept(context);
			} catch (Exception exc) {
				context.setCancelled(true);
				ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Fehler bei handleInventoryClick() von Spieler " + player.getName(), exc);
			}
		}
		event.setCancelled(context.isCancelled());
		if (context.isItemStackChanged()) {
			ItemStack newStack = context.getEffectiveItemStack();
			specialItems.put(clickedSlot, newStack);
			clickedInventory.setItem(clickedSlot < INDEX_DIFFERENCE ? clickedSlot : clickedSlot - INDEX_DIFFERENCE, newStack);
		}
		return true;
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, Inventory inventory, Consumer<Inventory> inventoryPreparer) {
		this.inventory = inventory;

		if (inventory == null) {
			throw new NullPointerException("Inventar für OpenableMenu nicht gesetzt. Nutze openInventorySync(Player, Inventory, Consumer<Inventory>) in Child-Klasse.");
		}

		if (player.isSleeping()) {
			player.wakeup(true);
		}

		// close actual inventories first so we don't override the original inventory
		InventoryHandler.getInstance().handleInventoryClose(player);

		saveInventory(player);
		for (int i = ROW_SIZE; i < (bottomRows + 1) * ROW_SIZE; i++) {
			player.getInventory().setItem(i, null);
		}

		for (int slot : slots) {
			ItemStack specialItem = specialItems.getOrDefault(slot, null);
			if (specialItem == null) {
				continue;
			}
			if (slot > INDEX_DIFFERENCE) {
				player.getInventory().setItem(slot - INDEX_DIFFERENCE, specialItem.clone());
			} else {
				inventory.setItem(slot, specialItem.clone());
			}
		}

		if (backHandler != null) {
			(backSlot > INDEX_DIFFERENCE ? player.getInventory() : inventory).setItem(
					backSlot > INDEX_DIFFERENCE ? backSlot - INDEX_DIFFERENCE : backSlot, DefaultSpecialItem.BACK.createSpecialItem());
		}

		if (inventoryPreparer != null) {
			try {
				inventoryPreparer.accept(inventory);
			} catch (Exception exc) {
				ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Fehler bei openInventorySync() von Spieler " + player.getName(), exc);
			}
		}

		InventoryView view = player.openInventory(inventory);
		if (view == null) {
			return null;
		}

		UUID playerId = player.getUniqueId();
		openInventories.put(playerId, inventory);
		InventoryHandler.getInstance().handleMenuOpen(player, this);

		return view;
	}

	@Override
	public boolean closeInventory(Player player) {
		openInventories.remove(player.getUniqueId());

		for (int i = ROW_SIZE; i < (bottomRows + 1) * ROW_SIZE; i++) {
			player.getInventory().setItem(i, null);
		}
		loadInventory(player);

		if (closeHandler == null) {
			return false;
		}

		try {
			closeHandler.accept(new CloseContext(player));
		} catch (Exception exc) {
			ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Fehler bei handleInventoryClose() von Spieler " + player.getName(), exc);
		}
		return true;
	}

	private int[] getSlotsFromRows(int rows, int bottomRows) {
		int size = rows * ROW_SIZE;
		int[] slots = new int[size + 3 * ROW_SIZE];
		for (int i = 0; i < size; i++) {
			slots[i] = i;
		}
		for (int i = ROW_SIZE; i < (bottomRows + 1) * ROW_SIZE; i++) {
			slots[i] = i + INDEX_DIFFERENCE;
		}
		return slots;
	}

	public void setSpecialItemBottom(int row, int column, @Nullable DefaultSpecialItem item) {
		setSpecialItem(calcIndexBottom(row, column), item);
	}

	public void setItemBottom(int index, @Nullable ItemStack stack) {
		setItem(index + INDEX_DIFFERENCE, stack);
	}

	public void setItemBottom(int row, int column, @Nullable ItemStack stack) {
		setItem(calcIndexBottom(row, column), stack);
	}

	public void setItemAndClickHandlerBottom(int index, DefaultSpecialItem item, @Nullable ContextConsumer<ClickContext> clickHandler) {
		this.setItemAndClickHandler(index + INDEX_DIFFERENCE, item, clickHandler);
	}

	public void setItemAndClickHandlerBottom(int row, int column, DefaultSpecialItem item, @Nullable ContextConsumer<ClickContext> clickHandler) {
		this.setItemAndClickHandler(calcIndexBottom(row, column), item.createSpecialItem(), clickHandler);
	}

	public void setItemAndClickHandlerBottom(int index, ItemStack item, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(index + INDEX_DIFFERENCE, item, clickHandler);
	}

	public void setItemAndClickHandlerBottom(int row, int column, ItemStack item, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(calcIndexBottom(row, column), item, clickHandler);
	}

	public void setItemAndClickHandlerBottom(int index, @Nullable ItemStack stack, ClickType action, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(index + INDEX_DIFFERENCE, stack, action, clickHandler);
	}

	public void setItemAndClickHandlerBottom(int row, int column, @Nullable ItemStack stack, ClickType action, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(calcIndexBottom(row, column), stack, action, clickHandler);
	}

	public void setItemAndClickHandlerBottom(int index, @Nullable ItemStack stack, @Nullable Map<ClickType, ContextConsumer<ClickContext>> clickHandler) {
		setItemAndClickHandler(index + INDEX_DIFFERENCE, stack, clickHandler);
	}

	public void setItemAndClickHandlerBottom(int row, int column, @Nullable ItemStack stack, @Nullable Map<ClickType, ContextConsumer<ClickContext>> clickHandler) {
		setItemAndClickHandler(calcIndexBottom(row, column), stack, clickHandler);
	}

	private int calcIndexBottom(int row, int column) {
		Preconditions.checkArgument(row >= 0, "bottom row muss >= 0 sein");
		Preconditions.checkArgument(row < bottomRows, "bottom row muss < " + bottomRows + " sein");
		Preconditions.checkArgument(column >= 0, "column muss >= 0 sein");
		Preconditions.checkArgument(column < ROW_SIZE, "column muss < " + ROW_SIZE + " sein");

		return ROW_SIZE + row * ROW_SIZE + column + INDEX_DIFFERENCE;
	}

	public void setBackSlotBottom(int row, int column) {
		super.setBackSlot(calcIndexBottom(row, column));
	}

	public void setBackSlotBottom(int backSlot) {
		super.setBackSlot(backSlot + ROW_SIZE + INDEX_DIFFERENCE);
	}

	public void fillBottom() {
		this.fillBottom(DefaultSpecialItem.EMPTY_DARK);
	}

	public void fillBottom(DefaultSpecialItem item) {
		for (int slot : getSlots()) {
			if (slot < INDEX_DIFFERENCE) {
				continue;
			}
			setItem(slot, item.createSpecialItem());
		}
	}

	public void refresh(Player player, int... slots) {
		for (int slot : slots) {
			Inventory inv = slot > INDEX_DIFFERENCE ? player.getInventory() : inventory;
			if (inv == null) {
				return;
			}
			inv.setItem(slot > INDEX_DIFFERENCE ? slot - INDEX_DIFFERENCE : slot, specialItems.getOrDefault(slot, null));
		}
	}
}
