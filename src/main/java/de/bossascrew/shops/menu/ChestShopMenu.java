package de.bossascrew.shops.menu;

import com.google.common.base.Preconditions;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.handler.InventoryHandler;
import de.bossascrew.shops.handler.LimitsHandler;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.util.LoggingPolicy;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;
import java.util.function.Consumer;

public class ChestShopMenu extends ChestMenu implements ShopMenu {

	private final ChestMenuShop shop;
	private @Nullable ContextConsumer<BackContext> customBackHandler;

	private final int cooldown;
	private final boolean showCooldownMessage;
	private final HashMap<Player, Long> interactionCooldown;

	public ChestShopMenu(ChestMenuShop shop) {
		this(shop, null);
	}

	public ChestShopMenu(ChestMenuShop shop, @Nullable ContextConsumer<BackContext> backHandler) {
		super(shop.getName(), shop.getRows(), 0, null, closeContext -> Customer.wrap(closeContext.getPlayer()).setActiveShop(null));
		this.shop = shop;
		this.customBackHandler = backHandler;
		this.interactionCooldown = new HashMap<>();
		this.cooldown = ShopPlugin.getInstance().getShopsConfig().getCooldown();
		this.showCooldownMessage = ShopPlugin.getInstance().getShopsConfig().isShowCooldownMessage();
	}

	@Override
	public InventoryView openInventory(Player player) {
		return openInventory(player, null);
	}

	public InventoryView openInventory(Player player, Consumer<Inventory> inventoryPreparer) {
		return openInventory(Customer.wrap(player));
	}

	public InventoryView openInventory(Customer customer) {
		return openInventory(customer, null);
	}

	public InventoryView openInventory(Customer customer, Consumer<Inventory> inventoryPreparer) {

		Preconditions.checkNotNull(customer, "customer");

		return ShopPlugin.getInstance().callTaskSync(() -> openInventorySync(customer.getPlayer(), null,
				shop.getPreferredShopMode(customer), shop.getPreferredOpenPage(customer)));
	}

	@Override
	public void setBackSlot(int backSlot) {
		//backhandler not allowed for ShopMenu
	}

	@Override
	protected void setBackHandlerAction(@NotNull ContextConsumer<BackContext> backHandler) {
		//backhandler not allowed for ShopMenu
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		return openInventorySync(player, inventoryPreparer, shop.getDefaultShopMode(), shop.getDefaultShopPage());
	}

	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer, ShopMode shopMode, int page) {
		Inventory inventory = Bukkit.createInventory(null, slots.length, Message.SHOP_GUI_TITLE.getLegacyTranslation(
				Template.of("name", shop.getName()),
				Template.of("page", "" + (page + 1)),
				Template.of("pages", "" + shop.getPageCount()),
				Template.of("mode", shopMode.getDisplayName())));
		return openInventorySync(player, inventory, inventoryPreparer, shopMode, page);
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, Inventory inventory, Consumer<Inventory> inventoryPreparer) {
		return openInventorySync(player, inventory, inventoryPreparer, shop.getDefaultShopMode(), shop.getDefaultShopPage());
	}

	public InventoryView openInventorySync(@NotNull Player player, Inventory inventory, Consumer<Inventory> inventoryPreparer, ShopMode shopMode, int page) {
		this.inventory = inventory;

		if (inventory == null) {
			throw new NullPointerException("Inventar für OpenableMenu nicht gesetzt. Nutze openInventorySync(Player, Inventory, Consumer<Inventory>) in Child-Klasse.");
		}

		if (player.isSleeping()) {
			player.wakeup(true);
		}

		if (inventoryPreparer != null) {
			try {
				inventoryPreparer.accept(inventory);
			} catch (Exception exc) {
				ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Fehler bei openInventorySync() von Spieler " + player.getName(), exc);
			}
		}

		fillMenu(DefaultSpecialItem.EMPTY_LIGHT);

		Map<Integer, ShopEntry> entries = shop.getModeEntryMap().getOrDefault(shopMode, new TreeMap<>());
		int pageSlots = shop.getRows() * RowedOpenableMenu.ROW_SIZE;

		for (int i = LARGEST_INV_SIZE * page; i < LARGEST_INV_SIZE * page + pageSlots; i++) {
			if (!entries.containsKey(i)) {
				continue;
			}
			ShopEntry entry = entries.get(i);
			if (entry == null) {
				ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Entry is null but contained in map at slot " + i);
				continue;
			}
			updateEntry(entry);

			//Subscribe to limits and discounts so changes can be displayed live
			DiscountHandler.getInstance().subscribeToDisplayUpdates(this, entry);
			LimitsHandler.getInstance().subscribeToDisplayUpdates(this, entry);
		}

		InventoryView view = player.openInventory(inventory);
		if (view == null) {
			return null;
		}

		Customer customer = Customer.wrap(player);
		customer.setPage(shop, page);
		customer.setMode(shop, shopMode);

		UUID playerId = player.getUniqueId();
		InventoryHandler.getInstance().handleMenuOpen(player, this);
		openInventories.put(playerId, inventory);

		return view;
	}

	@Override
	public boolean closeInventory(Player player) {
		DiscountHandler.getInstance().unsubscribeToDisplayUpdates(this);
		return super.closeInventory(player);
	}

	public void setEntry(ShopEntry entry) {

		int slot = entry.getSlot() % LARGEST_INV_SIZE;
		setItemAndClickHandler(slot, ItemStackUtils.createEntryItemStack(entry), clickContext -> {
			Player player = clickContext.getPlayer();
			Customer customer = Customer.wrap(player);

			long now = System.currentTimeMillis();
			Long last = interactionCooldown.get(player);
			if (last != null) {
				long dif = now - last;
				if (dif < cooldown) {
					if (showCooldownMessage) {
						customer.sendMessage(Message.SHOP_COOLDOWN);
					}
					return;
				}
			}
			entry.interact(customer);
			interactionCooldown.put(player, now);
		});
	}

	public void updateEntry(ShopEntry entry) {
		setEntry(entry);
		refresh(entry.getSlot() % LARGEST_INV_SIZE);
	}
}