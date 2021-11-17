package de.bossascrew.shops.menu;

import com.google.common.base.Preconditions;
import com.google.common.collect.Maps;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.contexts.ActionContext;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.CloseContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
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

/**
 * @param <T> entspricht dem Interaktionstypen. Ein Hotbarinventar basiert Beispielsweise auf dem PlayerInteractEvent und hat daher keinen
 *            InventoryAction Wert, auf den reagiert werden kann.
 */
public abstract class InventoryMenu<T, A extends ActionContext<T>, B extends BackContext, C extends CloseContext> {

	/**
	 * Alle für dieses Inventar möglichen Actions.
	 */
	@Getter
	protected final T[] actions;

	@Getter
	@Setter
	protected Map<T, ContextConsumer<A>> defaultClickHandler;

	@Getter
	@Setter
	protected @Nullable
	ContextConsumer<C> closeHandler;

	@Getter
	protected @Nullable
	ContextConsumer<B> backHandler;

	@Getter
	protected final Map<T, Boolean> defaultCancel;

	@Getter
	protected @Nullable
	Inventory inventory = null;

	@Getter
	protected final Map<Integer, ItemStack> specialItems = Maps.newHashMap();
	@Getter
	protected final Map<Integer, Map<T, ContextConsumer<A>>> clickHandlers = Maps.newHashMap();

	@Getter
	protected int[] slots;
	@Getter
	@Setter
	protected int backSlot;

	private final Map<UUID, Inventory> openInventories = Maps.newHashMap();


	public InventoryMenu(T[] actions, int[] slots) {
		this(actions, slots, 8, null, null);
	}

	public InventoryMenu(T[] actions, int[] slots, int backSlot, @Nullable Map<T, ContextConsumer<A>> defaultClickHandler,
						 @Nullable ContextConsumer<C> closeHandler) {

		this.actions = actions;
		this.slots = slots;
		this.backSlot = backSlot;
		this.defaultClickHandler = defaultClickHandler;
		this.closeHandler = closeHandler;
		this.defaultCancel = new HashMap<>();
		for (T action : actions) {
			this.defaultCancel.put(action, true);
		}

		if (this.defaultClickHandler == null) {
			this.defaultClickHandler = new HashMap<>();
		}
	}

	/**
	 * Setzt einen ItemStack in das Inventar. Bei Interaktion wird der DefaultClickHandler ausgeführt.
	 *
	 * @param slot Der Slot, an dem das Item gesetzt werden soll.
	 * @param item Das Item, das gesetzt werden soll.
	 */
	public void setSpecialItem(int slot, @Nullable DefaultSpecialItem item) {
		setItem(slot, item == null ? null : item.createSpecialItem());
	}

	/**
	 * Setzt einen ItemStack in das Inventar. Bei Interaktion wird der DefaultClickHandler ausgeführt.
	 *
	 * @param slot  Der Slot, an dem das Item gesetzt werden soll.
	 * @param stack Das Item, das gesetzt werden soll.
	 */
	public void setItem(int slot, @Nullable ItemStack stack) {
		specialItems.put(slot, stack);
	}

	/**
	 * Setzt ein Item in das Inventar, das bei Interaktion den angegebenen Clickhandler ausführt.
	 *
	 * @param slot         Der Slot, an den Item und Clickhandler gebunden werden sollen.
	 * @param item         Das Item, das an gegebenem Slot dargestellt werden soll.
	 * @param clickHandler Der Consumer, der ausgeführt wird, falls eine Interaktion stattfindet.
	 */
	public void setItemAndClickHandler(int slot, DefaultSpecialItem item, T action, @Nullable ContextConsumer<A> clickHandler) {
		setItemAndClickHandler(slot, item.createSpecialItem(), action, clickHandler);
	}

	public void setItemAndClickHandler(int slot, DefaultSpecialItem item, @Nullable ContextConsumer<A> clickHandler) {
		for (T action : actions) {
			setItemAndClickHandler(slot, item, action, clickHandler);
		}
	}

	public void setItemAndClickHandler(int slot, DefaultSpecialItem item, @Nullable Map<T, ContextConsumer<A>> clickHandler) {
		setItemAndClickHandler(slot, item.createSpecialItem(), clickHandler);
	}

	/**
	 * Setzt ein Item in das Inventar, das bei INteraktion den angegebenen Clickhandler ausführt.
	 *
	 * @param slot         Der Slot, an den Item und Clickhandler gebunden werden sollen.
	 * @param stack        Das Item, das an gegebenem Slot dargestellt werden soll.
	 * @param clickHandler Der Consumer, der ausgeführt wird, falls eine Interaktion stattfindet.
	 */
	public void setItemAndClickHandler(int slot, @Nullable ItemStack stack, T action, @Nullable ContextConsumer<A> clickHandler) {

		specialItems.put(slot, stack);
		setClickHandler(slot, action, clickHandler);
	}

	public void setItemAndClickHandler(int slot, ItemStack item, @Nullable ContextConsumer<A> clickHandler) {
		for (T action : actions) {
			setItemAndClickHandler(slot, item, action, clickHandler);
		}
	}

	public void setItemAndClickHandler(int slot, ItemStack item, Map<T, ContextConsumer<A>> clickHandler) {
		specialItems.put(slot, item);
		clickHandlers.put(slot, clickHandler);
	}

	public void setItemAndClickHandler(int slot, @Nullable ItemStack stack, T[] actions, @Nullable ContextConsumer<A> clickHandler) {
		setItem(slot, stack);
		for (T action : actions) {
			setClickHandler(slot, action, clickHandler);
		}
	}

	public void setClickHandler(int slot, @Nullable Map<T, ContextConsumer<A>> clickHandler) {
		clickHandlers.put(slot, clickHandler);
	}

	public void setClickHandler(int slot, T action, @Nullable ContextConsumer<A> clickHandler) {
		Map<T, ContextConsumer<A>> map = clickHandlers.getOrDefault(slot, new HashMap<>());
		map.put(action, clickHandler);
		clickHandlers.put(slot, map);
	}

	public void setClickHandler(int slot, @Nullable ContextConsumer<A> clickHandler) {
		for (T action : actions) {
			setClickHandler(slot, action, clickHandler);
		}
	}

	public void setClickHandler(int slot, T[] actions, @Nullable ContextConsumer<A> clickHandler) {
		for (T action : actions) {
			setClickHandler(slot, action, clickHandler);
		}
	}

	public void setDefaultClickHandler(T action, ContextConsumer<A> clickHandler) {
		defaultClickHandler.put(action, clickHandler);
	}

	public void setDefaultCancelled(boolean cancel) {
		for (T action : actions) {
			setDefaultCancelled(action, cancel);
		}
	}

	public void setDefaultCancelled(T action, boolean cancel) {
		defaultCancel.put(action, cancel);
	}

	public @Nullable
	ItemStack getItemStack(Integer slot) {
		if (inventory == null) {
			return null;
		}
		if (Arrays.stream(slots).noneMatch(value -> value == slot)) {
			return null;
		}
		return inventory.getItem(slot);
	}

	/**
	 * @param slot Der Slot, dessen zugehöriger ClickHandler zurückgegeben wird.
	 * @return Den im Menü definierten ClickHandler für den angegeben Slot.
	 */
	public @Nullable
	ContextConsumer<A> getClickHandler(int slot, T action) {
		return getClickHandler(slot).getOrDefault(action, null);
	}

	/**
	 * @param slot Der Slot, dessen zugehöriger ClickHandler zurückgegeben wird.
	 * @return Den im Menü definierten ClickHandler für den angegeben Slot.
	 */
	public Map<T, ContextConsumer<A>> getClickHandler(int slot) {
		if (Arrays.stream(slots).noneMatch(value -> value == slot)) {
			return null;
		}
		return clickHandlers.getOrDefault(slot, new HashMap<>());
	}

	/**
	 * Öffnet das Menü für den angegebenen Spieler.
	 *
	 * @return Das InventoryView Objekt für das geöffnete Menü.
	 */
	public InventoryView openInventory(Player player) {
		return openInventory(player, null);
	}

	/**
	 * Öffnet das Menü für den angegebenen Spieler
	 *
	 * @param player            Der Spieler, für den das Menü geöffnet werden soll.
	 * @param inventoryPreparer Der inventoryPreparer bietet die Möglichkeit, das Inventar im Vorhinein anzupassen.
	 * @return Das InventoryView Objekt für das geöffnete Menü.
	 */
	public InventoryView openInventory(Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		Preconditions.checkNotNull(player, "player");

		return ShopPlugin.getInstance().callTaskSync(() -> openInventorySync(player, inventoryPreparer));
	}

	/**
	 * Öffnet das Menü synchron
	 */
	public abstract InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer);

	/**
	 * Schließt das Inventar. Dies wird in den meisten Fällen player.closeInventory() sein, gilt aber beispielsweise nicht bei Hotbarmenüs.
	 *
	 * @return true, wenn der closeHandler gesetzt war und ausgeführt wurde.
	 */
	public abstract boolean closeInventory(Player player);

	/**
	 * Führt den Aktionsaufruf für das Inventar aus.
	 *
	 * @return true, wenn die Behandlung erfolgreich abgeschlossen wurde.
	 */
	public abstract boolean handleAction(Player player, Event event);

	/**
	 * Befüllt das gesamte Inventar mit hellgrauen Glasscheiben (DefaultSpecialItem.EMPTY_LIGHT)
	 */
	public void fillMenu() {
		fillMenu(DefaultSpecialItem.EMPTY_LIGHT);
	}

	/**
	 * Befüllt das gesamte Inventar mit dunkelgrauen Glasscheiben (DefaultSpecialItem.EMPTY_DARK)
	 */
	public void fillMenuDark() {
		fillMenu(DefaultSpecialItem.EMPTY_DARK);
	}

	/**
	 * Befüllt das ganze Inventar mit angegebenem DefaultSpecialItem.
	 *
	 * @param fillItem das zu befüllende Item
	 */
	public void fillMenu(DefaultSpecialItem fillItem) {
		Preconditions.checkNotNull(fillItem, "fillItem");
		fillMenu(backHandler, fillItem);
	}

	/**
	 * Befüllt das ganze Inventar mit angegebenem DefaultSpecialItem und setzt anschließend den backHandler an den BackHandlerSlot.
	 *
	 * @param backHandler Die Runnable, die ausgeführt wird, wenn der Spieler mit dem BackHandlerItem interagiert.
	 * @param fillItem    Das Item, das in alle Inventarslots gelegt werden soll.
	 */
	public void fillMenu(@Nullable ContextConsumer<B> backHandler, DefaultSpecialItem fillItem) {
		Preconditions.checkNotNull(fillItem, "fillItem");
		ItemStack fillItemStack = fillItem.createSpecialItem();

		Arrays.stream(getSlots()).forEach(index -> setItem(index, fillItemStack));
		if (backHandler != null) {
			setBackHandlerAction(backHandler);
		}
	}

	public void fillMenu(ContextConsumer<B> backHandler) {
		fillMenu(backHandler, DefaultSpecialItem.EMPTY_LIGHT);
	}

	@Deprecated
	public void setBackHandler(@Nullable ContextConsumer<B> backHandler) {
		this.backHandler = backHandler;
		setSpecialItem(backSlot, DefaultSpecialItem.BACK);
		if (backHandler != null) {
			setBackHandlerAction(backHandler);
		}
	}

	public @Nullable
	ContextConsumer<A> getOrDefaultHandler(int slot, T action) {
		ContextConsumer<A> clickHandler = clickHandlers.getOrDefault(slot, new HashMap<>()).getOrDefault(action, null);
		if (clickHandler == null) {
			clickHandler = defaultClickHandler.getOrDefault(action, null);
		}
		return clickHandler;
	}

	protected abstract void setBackHandlerAction(@NotNull ContextConsumer<B> backHandler);
}