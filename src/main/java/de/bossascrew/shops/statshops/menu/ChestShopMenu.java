package de.bossascrew.shops.statshops.menu;

import com.google.common.base.Preconditions;
import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.menu.ChestMenu;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.general.util.TradeMessageType;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.InventoryHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import de.bossascrew.shops.statshops.shop.ShopMode;
import net.kyori.adventure.text.Component;
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
		this.cooldown = StatShops.getInstance().getShopsConfig().getCooldown();
		this.showCooldownMessage = StatShops.getInstance().getShopsConfig().isShowCooldownMessage();
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

		return StatShops.getInstance().callTaskSync(() -> openInventorySync(customer.getPlayer(), null,
				shop.getPreferredShopMode(customer), shop.getPreferredOpenPage(customer)));
	}

	@Override
	public void setBackSlot(int backSlot) {
		//backhandler not allowed for ShopMenu
	}

	@Override
	public void setBackHandlerAction(@NotNull ContextConsumer<BackContext> backHandler) {
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
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Fehler bei openInventorySync() von Spieler " + player.getName(), exc);
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
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Entry is null but contained in map at slot " + i);
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

		//TODO trade chacher object pro Shop, hier nur trigger aufrufen
		Customer customer = Customer.wrap(player);
		if (shop.getTradeCache().getOrDefault(customer, new HashMap<>()).size() > 0) {
			customer.sendMessage(Message.SHOP_TRADE_FEEDBACK_CUMUL_TITLE, 0);
		}
		for (Map.Entry<Component, Double> entry : shop.getTradeCache().getOrDefault(customer, new HashMap<>()).entrySet()) {
			customer.sendMessage("", StatShops.getInstance().getTransactionFeedback(entry.getValue(), entry.getKey(), false), 0);
		}
		return super.closeInventory(player);
	}

	public <T> void setEntry(ShopEntry entry) {

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
			ShopInteractionResult result = entry.interact(customer);
			if (result == ShopInteractionResult.SUCCESS && entry.getModule() != null && entry.getModule() instanceof TradeModule tm) {
				//TODO ganz viel nach currency und reward auslagern

				double priceAmount = tm.getPriceAmount() * -1;
				int gainAmount = tm.getArticle().getAmount();

				TradeMessageType feedback = StatShops.getInstance().getShopsConfig().getTradeMessageFeedback();
				Map<Component, Double> innerMap = shop.getTradeCache().getOrDefault(customer, new HashMap<>());

				Component gainComponent = TextUtils.toComponent(tm.getArticle());
				double setGain = innerMap.getOrDefault(gainComponent, 0.);

				Component priceComponent = tm.getCurrency().getCurrencyComponent(2, tm.getPriceObject());
				double setPrice = innerMap.getOrDefault(priceComponent, 0.);

				if (feedback.equals(TradeMessageType.CUMULATIVE)) {
					innerMap.put(gainComponent, setGain + gainAmount);
					innerMap.put(priceComponent, setPrice + priceAmount);

				} else if (feedback.equals(TradeMessageType.PROMPT)) {
					customer.sendMessage("", StatShops.getInstance().getTransactionFeedback(gainAmount, gainComponent, true), 0);
					customer.sendMessage("", StatShops.getInstance().getTransactionFeedback(priceAmount, priceComponent, true), 0);
				}
				shop.getTradeCache().put(customer, innerMap);
			}
			interactionCooldown.put(player, now);
		});
	}

	public void updateEntry(ShopEntry entry) {
		setEntry(entry);
		refresh(entry.getSlot() % LARGEST_INV_SIZE);
	}
}
