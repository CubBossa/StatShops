package de.bossascrew.shops.statshops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.PaginatedShop;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.TransactionBalanceMessenger;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.events.ShopCloseEvent;
import de.bossascrew.shops.statshops.events.ShopOpenEvent;
import de.bossascrew.shops.statshops.events.ShopTurnPageEvent;
import de.bossascrew.shops.statshops.menu.ChestShopEditor;
import de.bossascrew.shops.statshops.menu.ChestShopMenu;
import de.bossascrew.shops.statshops.shop.entry.BaseEntry;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

@Getter
@Setter
public class ChestMenuShop implements PaginatedShop {

	private final UUID uuid;
	private String nameFormat;

	@JsonIgnore
	private Component name;
	@JsonIgnore
	private String namePlain;

	@JsonIgnore
	private ItemStack displayItem;
	private @Nullable String permission = null;
	private @Nullable EntryTemplate defaultTemplate = null;
	private TransactionBalanceMessenger balanceMessenger;
	private final List<UUID> pageTurningPlayers;

	private int rows = 3;

	private boolean isPageRemembered = false;
	private int defaultPage = 0;
	@JsonIgnore
	private @Nullable Player editor = null;

	@JsonIgnore
	private final Map<UUID, ShopEntry> uuidEntryMap;
	private final TreeMap<Integer, ShopEntry> entryMap;
	/**
	 * contains all entries that were removed via editor. When later on an item with the uuid tag of this entry was added, it can be restored from cache.
	 */
	@JsonIgnore
	private final Map<UUID, ShopEntry> unusedEntryCache;

	@JsonIgnore
	private final List<Customer> activeCustomers;
	private final Map<Customer, ChestShopMenu> menuMap;
	private final List<String> tags;
	private final Map<Integer, String> pageTitles;

	public ChestMenuShop(String nameFormat) {
		this(nameFormat, UUID.randomUUID());
	}

	public ChestMenuShop(String nameFormat, UUID uuid) {
		setNameFormat(nameFormat);
		this.uuid = uuid;

		this.uuidEntryMap = new HashMap<>();
		this.entryMap = new TreeMap<>();
		this.unusedEntryCache = new HashMap<>();
		this.activeCustomers = new ArrayList<>();
		this.menuMap = new HashMap<>();
		this.tags = new ArrayList<>();
		this.balanceMessenger = new SimpleBalanceMessenger(StatShops.getInstance().getShopsConfig().getTradeMessageFeedback());
		this.pageTurningPlayers = new ArrayList<>();
		this.pageTitles = new HashMap<>();
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = TextUtils.toPlain(this.name);
	}

	@Override
	public int getPageCount() {
		//important to divide with largest inventory size so entries dont move to other pages when changing the row size
		return entryMap.isEmpty() ? 1 : entryMap.lastKey() / RowedOpenableMenu.LARGEST_INV_SIZE + 1;
	}

	public @Nullable
	ShopEntry getEntry(int slot) {
		return entryMap.getOrDefault(slot, null);
	}

	@Override
	public boolean removeEntry(ShopEntry shopEntry) {
		StatShops.getInstance().getDatabase().deleteEntry(shopEntry);
		return entryMap.remove(shopEntry.getSlot(), shopEntry);
	}

	@Override
	public ShopEntry getEntry(UUID uuid) {
		return uuidEntryMap.get(uuid);
	}

	@Override
	public ShopEntry getUnusedEntry(UUID uuid) {
		return unusedEntryCache.get(uuid);
	}

	@Override
	public List<ShopEntry> getEntries(int shopPage) {
		int lowerBound = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE;
		int upperBound = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE + rows * RowedOpenableMenu.ROW_SIZE;

		return entryMap.entrySet().stream()
				.filter(e -> e.getKey() >= lowerBound && e.getKey() < upperBound)
				.map(Map.Entry::getValue)
				.collect(Collectors.toList());
	}

	@Override
	public ShopEntry createEntry(ItemStack displayItem, int slot) {
		ShopEntry entry = new BaseEntry(UUID.randomUUID(), this, displayItem.clone(), null, slot);
		ShopEntry oldEntry = entryMap.put(slot, entry);
		if (oldEntry != null) {
			StatShops.getInstance().getDatabase().deleteEntry(oldEntry);
		}
		StatShops.getInstance().getDatabase().saveEntry(entry);
		uuidEntryMap.put(entry.getUUID(), entry);
		return entry;
	}

	public ShopEntry addEntry(ShopEntry entry, int slot) {
		ShopEntry oldEntry = entryMap.put(slot, entry);
		if (oldEntry != null) {
			StatShops.getInstance().getDatabase().deleteEntry(oldEntry);
		}
		uuidEntryMap.put(entry.getUUID(), entry);
		unusedEntryCache.remove(entry.getUUID());
		return entry;
	}

	@Override
	public boolean moveEntry(ShopEntry entry, int slot) {
		if (getEntry(entry.getUUID()) == null) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Tried to move an entry that was not contained in this shop.");
			return false;
		}
		boolean override = false;
		ShopEntry oldEntry = getEntry(slot);
		if (oldEntry != null) {
			if (oldEntry.equals(entry)) {
				return false;
			}
			setEntryUnused(oldEntry);
			override = true;
		}
		entry.setSlot(slot);
		addEntry(entry, slot);
		return override;
	}

	@Override
	public boolean deleteEntry(int slot) {
		return deleteEntry(getEntry(slot));
	}

	@Override
	public boolean deleteEntry(ShopEntry entry) {
		if (entry == null) {
			return false;
		}
		uuidEntryMap.remove(entry.getUUID());
		StatShops.getInstance().getDatabase().deleteEntry(entry);
		return entryMap.remove(entry.getSlot(), entry);
	}

	@Override
	public boolean setEntryUnused(ShopEntry entry) {
		StatShops.getInstance().getDatabase().deleteEntry(entry);
		unusedEntryCache.put(entry.getUUID(), entry);
		return entryMap.remove(entry.getSlot(), entry);
	}

	@Override
	public void applyTemplate(EntryTemplate template) {
		throw new UnsupportedOperationException("Use applyTemplate(EntryTemplate t, int page) with page parameter to apply template to pagination");
	}

	@Override
	public void setPageRemembered(boolean rememberPage) {
		this.isPageRemembered = rememberPage;
	}

	public int getPreferredOpenPage(Customer customer) {
		return isPageRemembered ? customer.getPage(this, defaultPage) : defaultPage;
	}

	@Override
	public int getDefaultShopPage() {
		return defaultPage;
	}

	@Override
	public void setDefaultShopPage(int page) {
		this.defaultPage = page;
	}

	@Override
	public Component getPageTitle(int page) {
		if (!pageTitles.containsKey(page)) {
			return Component.empty();
		}
		return StatShops.getInstance().getMiniMessage().parse(pageTitles.get(page));
	}

	public String getPageTitleFormat(int page) {
		return pageTitles.getOrDefault(page, "");
	}

	@Override
	public void setPageTitle(int page, String titleFormat) {
		pageTitles.put(page, titleFormat);
	}

	public boolean open(Customer customer) {
		return open(customer, getPreferredOpenPage(customer));
	}

	@Override
	public boolean open(Customer customer, ContextConsumer<CloseContext> closeHandler) {
		return open(customer, getPreferredOpenPage(customer), closeHandler);
	}

	public boolean open(Customer customer, int page) {
		return open(customer, page, null);
	}

	@Override
	public boolean open(Customer customer, int page, @Nullable ContextConsumer<CloseContext> closeHandler) {
		if (editor != null && !editor.getUniqueId().equals(customer.getUuid())) {
			customer.sendMessage(Message.SHOP_NOT_ENABLED);
			return false;
		}
		if (permission != null && !customer.getPlayer().hasPermission(permission)) {
			customer.sendMessage(Message.SHOP_NO_PERMISSION);
			return false;
		}
		if (!(page >= 0 && page < getPageCount())) {
			return false;
		}
		if (pageTurningPlayers.contains(customer.getUuid())) {
			pageTurningPlayers.remove(customer.getUuid());
			if (!handleShopTurnPage(customer, page)) {
				return false;
			}
		} else {
			if (!handleShopOpen(customer, page)) {
				return false;
			}
		}
		if (closeHandler == null && menuMap.containsKey(customer)) {
			closeHandler = menuMap.get(customer).getCloseHandler();
		}
		@Nullable ContextConsumer<CloseContext> finalCloseHandler = closeHandler;

		ChestShopMenu menu = new ChestShopMenu(this, customer, null);
		menu.setCloseHandler(closeContext -> {
			if (!pageTurningPlayers.contains(customer.getUuid())) {
				if (finalCloseHandler == null) {
					if (!handleShopClose(customer)) {
						open(customer, page, null);
					}
				} else {
					finalCloseHandler.accept(closeContext);
				}
			}
		});
		menu.openInventorySync(customer.getPlayer(), null, page);
		menuMap.put(customer, menu);

		activeCustomers.add(customer);
		customer.setActiveShop(this);

		return true;
	}

	private boolean handleShopOpen(Customer customer, int page) {
		ShopOpenEvent shopOpenEvent = new ShopOpenEvent(this, customer, page);
		Bukkit.getPluginManager().callEvent(shopOpenEvent);
		return !shopOpenEvent.isCancelled();
	}

	private boolean handleShopClose(Customer customer) {
		ShopCloseEvent event = new ShopCloseEvent(this, 0); //TODo page
		Bukkit.getPluginManager().callEvent(event);

		if (!event.isCancelled()) {
			balanceMessenger.handleShopClose(customer.getPlayer());
			activeCustomers.remove(customer);
			return true;
		}
		return false;
	}

	private boolean handleShopTurnPage(Customer customer, int page) {
		ShopTurnPageEvent turnPageEvent = new ShopTurnPageEvent(this, customer, page);
		Bukkit.getPluginManager().callEvent(turnPageEvent);
		if (turnPageEvent.isCancelled()) {
			return false;
		}
		balanceMessenger.handlePageClose(customer.getPlayer());
		return true;
	}

	public void announceTurnPage(Customer customer) {
		pageTurningPlayers.add(customer.getUuid());
	}

	public boolean close(Customer customer) {
		ChestShopMenu menu = menuMap.get(customer);
		if (menu != null) {
			if (customer.getPlayer().getOpenInventory().getTopInventory().equals(menu.getInventory())) {
				customer.getPlayer().closeInventory();
			}
			menuMap.remove(customer);
		}
		customer.setActiveShop(null);
		return activeCustomers.remove(customer);
	}

	public void closeAll() {
		for (Customer customer : activeCustomers) {
			close(customer);
		}
	}

	@Override
	public void openEditorMenu(Player player, ContextConsumer<BackContext> backHandler) {
		new ChestShopEditor(this, backHandler).openInventory(player, getDefaultShopPage());
	}

	public EntryInteractionResult interact(Customer customer, int slot, EntryInteractionType interactionType) {
		if (editor != null && !customer.getUuid().equals(editor.getUniqueId())) {
			return EntryInteractionResult.FAIL_SHOP_DISABLED;
		}
		ShopEntry entry = getEntry(slot);
		if (entry == null) {
			return EntryInteractionResult.FAIL_NO_ENTRY;
		}
		ShopMenu menu = menuMap.get(customer);
		return entry.interact(customer, menu, interactionType);
	}

	public void setRows(int rows) {
		this.rows = rows % 6;
		if(this.rows <= 0) {
			this.rows += 6;
		}
		//all rows between 1 - 6
	}

	@Override
	public UUID getUUID() {
		return uuid;
	}

	public void setPermission(String permission) {
		this.permission = permission != null ? permission.equalsIgnoreCase("null") ? null : permission : null;
	}

	@Override
	public Map<Integer, ShopEntry> getEntries() {
		return entryMap;
	}

	public List<String> getTags(boolean generated) {
		return generated ? getTags() : new ArrayList<>(tags);
	}

	public List<String> getTags() {
		List<String> list = new ArrayList<>(tags);
		list.add(uuid.toString());
		list.add(StatShops.TAG_GLOBAL);
		list.sort(String::compareTo);
		return list;
	}

	@Override
	public boolean addTag(String tag) {
		if (tags.contains(tag)) {
			return false;
		}
		return tags.add(tag);
	}

	@Override
	public boolean removeTag(String tag) {
		return tags.remove(tag);
	}

	@Override
	public boolean hasTag(String tag) {
		return getTags().contains(tag);
	}

	@Override
	public void applyTemplate(EntryTemplate template, int shopPage) {
		for (ShopEntry entry : template.getEntries(rows).values()) {
			int shopSlot = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE + entry.getSlot();
			ShopEntry newEntry = entry.duplicate();
			newEntry.setSlot(shopSlot);
			newEntry.setShop(this);
			addEntry(newEntry, shopSlot);
			newEntry.saveToDatabase();
		}
	}

	@Override
	public void applyDefaultTemplate(EntryTemplate template, int shopPage) {
		for (int page = 0; page <= shopPage; page++) {
			// Only override entries on actual page and not on any page before
			if (page != shopPage && getEntries(page).size() > 0) {
				continue;
			}
			for (ShopEntry entry : template.getEntries(rows).values()) {
				int shopSlot = page * RowedOpenableMenu.LARGEST_INV_SIZE + entry.getSlot();
				if (getEntry(shopSlot) != null) {
					continue;
				}
				ShopEntry newEntry = entry.duplicate();
				newEntry.setSlot(shopSlot);
				newEntry.setShop(this);
				newEntry.saveToDatabase();
				addEntry(newEntry, shopSlot);
			}
		}
	}

	@Override
	public int compareTo(@NotNull Shop o) {
		return namePlain.compareTo(o.getNamePlain());
	}

	public ItemStack getDisplayItem() {
		return displayItem == null ? null : displayItem.clone();
	}

	@Override
	@JsonIgnore
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createShopItemStack(this);
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().runAsync(() -> StatShops.getInstance().getDatabase().saveShop(this));
	}
}
