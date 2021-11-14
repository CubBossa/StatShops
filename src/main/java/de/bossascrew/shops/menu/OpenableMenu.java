package de.bossascrew.shops.menu;

import com.google.common.collect.Maps;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.InventoryHandler;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ClickContext;
import de.bossascrew.shops.menu.contexts.CloseContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.event.player.PlayerSwapHandItemsEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

public abstract class OpenableMenu extends InventoryMenu<ClickType, ClickContext, BackContext, CloseContext> {

	@Getter
	@Setter
	protected Component title;

	@Getter
	protected final InventoryType type;

	@Getter
	protected final Map<UUID, Inventory> openInventories = Maps.newHashMap();

	public OpenableMenu(Component title, InventoryType type, int[] slots, int backSlot, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler,
						@Nullable ContextConsumer<CloseContext> closeHandler) {
		super(ClickType.values(), slots, backSlot, defaultClickHandler, closeHandler);

		this.title = title;
		this.type = type;
	}

	public InventoryView openInventorySync(@NotNull Player player, Inventory inventory, Consumer<Inventory> inventoryPreparer) {
		this.inventory = inventory;

		if (inventory == null) {
			throw new NullPointerException("Inventar für OpenableMenu nicht gesetzt. Nutze openInventorySync(Player, Inventory, Consumer<Inventory>) in Child-Klasse.");
		}

		if (player.isSleeping()) {
			player.wakeup(true);
		}

		for (int slot : slots) {
			ItemStack specialItem = specialItems.getOrDefault(slot, null);
			if (specialItem == null) {
				continue;
			}
			inventory.setItem(slot, specialItem.clone());
		}

		if (backHandler != null) {
			inventory.setItem(backSlot, DefaultSpecialItem.BACK.createSpecialItem());
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
		InventoryHandler.getInstance().getOpenOpenableMenus().put(playerId, this);
		openInventories.put(playerId, inventory);

		return view;
	}

	@Override
	public boolean closeInventory(Player player) {
		openInventories.remove(player.getUniqueId());

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

	@Override
	public boolean handleAction(Player player, Event anyEvent) {
		if (anyEvent instanceof PlayerSwapHandItemsEvent) {
			((PlayerSwapHandItemsEvent) anyEvent).setCancelled(true);
			return false;
		}
		if (anyEvent instanceof InventoryDragEvent) {
			if (((InventoryDragEvent) anyEvent).getInventory().equals(inventory)) {
				((InventoryDragEvent) anyEvent).setCancelled(true);
				return false;
			}
		}
		if (!(anyEvent instanceof InventoryClickEvent)) {
			ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "OpenableMenu mit falschem Event aufgerufen: " + anyEvent.getClass().getName());
			return false;
		}
		return handleAction(player, anyEvent);
	}

	public boolean handleAction(Player player, InventoryClickEvent event) {

		if (event.getClickedInventory() == null || !event.getClickedInventory().equals(inventory)) {
			return false;
		}
		int clickedSlot = event.getSlot();
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
			inventory.setItem(clickedSlot, newStack);
		}
		return true;
	}

	public void refresh(int... slots) {
		if (inventory == null) {
			return;
		}
		for (int slot : slots) {
			inventory.setItem(slot, specialItems.getOrDefault(slot, null));
		}
	}

	@Override
	protected void setBackHandlerAction(@NotNull ContextConsumer<BackContext> backHandler) {
		this.backHandler = backHandler;
		setClickHandler(backSlot, c -> backHandler.accept(new BackContext(c.getPlayer())));
	}
}