package de.bossascrew.shops.statshops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import de.bossascrew.shops.general.*;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.menu.ChestShopEditor;
import de.bossascrew.shops.statshops.menu.ChestShopMenu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

@Getter
@Setter
public class ChestMenuShop implements ModedShop, PaginatedShop, PaginatedModedShop {

	private final UUID uuid;
	private String nameFormat;

	@JsonIgnore
	private Component name;
	@JsonIgnore
	private String namePlain;

	@JsonIgnore
	private Material displayMaterial;
	private @Nullable String permission = null;
	private @Nullable EntryTemplate defaultTemplate = null;

	private int rows = 3;

	private boolean isPageRemembered = false;
	private boolean isModeRemembered = false;
	private int defaultPage = 0;
	private ShopMode defaultShopMode = null;
	@JsonIgnore
	private @Nullable Player editor = null;

	@JsonIgnore
	private final Map<UUID, ShopEntry> uuidEntryMap;
	private final Map<ShopMode, TreeMap<Integer, ShopEntry>> modeEntryMap;
	/**
	 * contains all entries that were removed via editor. When later on an item with the uuid tag of this entry was added, it can be restored from cache.
	 */
	@JsonIgnore
	private final Map<UUID, ShopEntry> unusedEntryCache;

	@JsonIgnore
	private final List<Customer> activeCustomers;

	private final Map<Customer, ChestShopMenu> menuMap;

	private final List<String> tags;

	public ChestMenuShop(String nameFormat) {
		this(nameFormat, UUID.randomUUID());
	}

	public ChestMenuShop(String nameFormat, UUID uuid) {
		setNameFormat(nameFormat);
		this.uuid = uuid;

		this.uuidEntryMap = new HashMap<>();
		this.modeEntryMap = new HashMap<>();
		this.unusedEntryCache = new HashMap<>();
		this.activeCustomers = new ArrayList<>();
		this.menuMap = new HashMap<>();
		this.tags = new ArrayList<>();
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = TextUtils.toPlain(this.name);
	}

	@Override
	public int getPageCount() {
		int highest = 0;
		for (TreeMap<Integer, ShopEntry> map : modeEntryMap.values()) {
			int h = map.lastKey();
			if (h > highest) {
				highest = h;
			}
		}
		//important to divide with largest inventory size so entries dont move to other pages when changing the row size
		return highest / RowedOpenableMenu.LARGEST_INV_SIZE + 1;
	}

	public @Nullable
	ShopEntry getEntry(ShopMode shopEntry, int slot) {
		return modeEntryMap.getOrDefault(shopEntry, new TreeMap<>()).getOrDefault(slot, null);
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
	public List<ShopEntry> getEntries(ShopMode shopMode, int shopPage) {
		int lowerBound = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE;
		int upperBound = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE + rows * RowedOpenableMenu.ROW_SIZE;

		List<ShopEntry> entries;
		Map<Integer, ShopEntry> inner = modeEntryMap.get(shopMode);
		if (inner != null) {
			entries = inner.entrySet().stream()
					.filter(e -> e.getKey() >= lowerBound && e.getKey() < upperBound)
					.map(Map.Entry::getValue)
					.collect(Collectors.toList());
		} else {
			entries = new ArrayList<>();
		}
		return entries;
	}

	@Override
	public ShopEntry createEntry(ItemStack displayItem, ShopMode shopMode, int slot) {
		ShopEntry entry = StatShops.getInstance().getDatabase().createEntry(UUID.randomUUID(), this, displayItem, shopMode, slot);
		TreeMap<Integer, ShopEntry> innerMap = modeEntryMap.getOrDefault(shopMode, new TreeMap<>());
		innerMap.put(slot, entry);
		modeEntryMap.put(shopMode, innerMap);
		uuidEntryMap.put(entry.getUUID(), entry);
		return entry;
	}

	public ShopEntry addEntry(ShopMode shopMode, int slot, ShopEntry entry) {
		TreeMap<Integer, ShopEntry> innerMap = modeEntryMap.getOrDefault(shopMode, new TreeMap<>());
		innerMap.put(slot, entry);
		modeEntryMap.put(shopMode, innerMap);
		uuidEntryMap.put(entry.getUUID(), entry);
		unusedEntryCache.remove(entry.getUUID());
		return entry;
	}

	@Override
	public boolean moveEntry(ShopEntry entry, ShopMode shopMode, int slot) {
		if (getEntry(entry.getUUID()) == null) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Tried to move an entry that was not contained in this shop.");
			return false;
		}
		boolean override = getEntry(shopMode, slot) != null;
		entry.setShopMode(shopMode);
		entry.setSlot(slot);
		addEntry(shopMode, slot, entry);
		return override;
	}

	@Override
	public boolean deleteEntry(ShopMode shopMode, int slot) {
		return deleteEntry(getEntry(shopMode, slot));
	}

	@Override
	public boolean deleteEntry(ShopEntry entry) {
		if (entry == null) {
			return false;
		}
		uuidEntryMap.remove(entry.getUUID());
		StatShops.getInstance().getDatabase().deleteEntry(entry);
		TreeMap<Integer, ShopEntry> innerMap = modeEntryMap.get(entry.getShopMode());
		if (innerMap == null) {
			return false;
		}
		return innerMap.remove(entry.getSlot(), entry);
	}

	@Override
	public boolean setEntryUnused(ShopEntry entry) {
		TreeMap<Integer, ShopEntry> innerMap = modeEntryMap.get(entry.getShopMode());
		if (innerMap == null) {
			return false;
		}
		unusedEntryCache.put(entry.getUUID(), entry);
		return innerMap.remove(entry.getSlot(), entry);
	}

	@Override
	public void applyTemplate(EntryTemplate template) {

	}

	@Override
	public void setPageRemembered(boolean rememberPage) {
		this.isPageRemembered = rememberPage;
	}

	public int getPreferredOpenPage(Customer customer) {
		return isPageRemembered ? customer.getPage(this, defaultPage) : defaultPage;
	}

	@Override
	public void setModeRemembered(boolean rememberMode) {
		this.isModeRemembered = rememberMode;
	}

	@Override
	public ShopMode getDefaultShopMode() {
		return defaultShopMode;
	}

	@Override
	public void setDefaultShopMode(ShopMode shopMode) {
		this.defaultShopMode = shopMode;
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
	public void applyTemplate(EntryTemplate template, int shopPage) {

	}

	public @Nullable
	ShopMode getPreferredShopMode(Customer customer) {
		ShopMode mode = isModeRemembered ? customer.getShopMode(this, defaultShopMode) : defaultShopMode;
		if (mode == null) {
			StatShops.getInstance().log(LoggingPolicy.WARN, "Default ShopMode was null, using first registered mode.");
			return ShopHandler.getInstance().getShopModes().get(0);
		}
		return mode;
	}

	@Override
	public void applyTemplate(EntryTemplate template, ShopMode shopMode) {
		//TODO
	}

	public boolean open(Customer customer) {
		return open(customer, getPreferredOpenPage(customer), getPreferredShopMode(customer));
	}

	@Override
	public boolean open(Customer customer, ContextConsumer<BackContext> backHandler) {
		return open(customer, getPreferredOpenPage(customer), getPreferredShopMode(customer), backHandler);
	}

	public boolean open(Customer customer, int page) {
		return open(customer, page, getPreferredShopMode(customer));
	}

	@Override
	public boolean open(Customer customer, int page, ContextConsumer<BackContext> backHandler) {
		return open(customer, page, getPreferredShopMode(customer), backHandler);
	}

	public boolean open(Customer customer, ShopMode mode) {
		return open(customer, getPreferredOpenPage(customer), mode);
	}

	@Override
	public boolean open(Customer customer, ShopMode shopMode, ContextConsumer<BackContext> backHandler) {
		return open(customer, getPreferredOpenPage(customer), shopMode, backHandler);
	}

	public boolean open(Customer customer, int page, ShopMode mode) {
		return open(customer, page, mode, null);
	}

	@Override
	public boolean open(Customer customer, int page, ShopMode shopMode, @Nullable ContextConsumer<BackContext> backHandler) {
		if (editor != null && !editor.getUniqueId().equals(customer.getUuid())) {
			customer.sendMessage(Message.SHOP_NOT_ENABLED);
			return false;
		}
		if (permission != null && !customer.getPlayer().hasPermission(permission)) {
			customer.sendMessage(Message.SHOP_NO_PERMISSION);
			return false;
		}
		ChestShopMenu menu = new ChestShopMenu(this, backHandler);
		menu.openInventorySync(customer.getPlayer(), null, shopMode, page);
		menuMap.put(customer, menu);

		customer.setActiveShop(this);
		activeCustomers.add(customer);

		return true;
	}

	public boolean close(Customer customer) {
		ChestShopMenu menu = menuMap.get(customer);
		if (menu != null) {
			if (customer.getPlayer().getOpenInventory().getTopInventory().equals(menu.getInventory())) {
				customer.getPlayer().closeInventory();
			}
			menuMap.remove(customer);
		}
		return activeCustomers.remove(customer);
	}

	public void closeAll() {
		for (Customer customer : activeCustomers) {
			close(customer);
		}
	}

	@Override
	public void openEditorMenu(Player player, ContextConsumer<BackContext> backHandler) {
		new ChestShopEditor(this, backHandler).openInventory(player, getDefaultShopMode(), getDefaultShopPage());
	}

	public ShopInteractionResult interact(Customer customer, ShopMode shopMode, int slot) {
		if (editor != null && !customer.getUuid().equals(editor.getUniqueId())) {
			return ShopInteractionResult.FAIL_SHOP_DISABLED;
		}
		ShopEntry entry = getEntry(shopMode, slot);
		if (entry == null) {
			return ShopInteractionResult.FAIL_NO_ENTRY;
		}
		if (!entry.hasPermission(customer)) {
			return ShopInteractionResult.FAIL_NO_PERMISSION;
		}
		return entry.interact(customer);
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
	public void newEntry(int slot, ShopEntry entry) {
		//TODO
	}

	@Override
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
	public void applyTemplate(EntryTemplate template, ShopMode shopMode, int shopPage) {
		for (ShopEntry entry : template.getEntries(rows).values()) {
			int shopSlot = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE + entry.getSlot();
			ShopEntry newEntry = entry.duplicate();
			newEntry.setShopMode(shopMode);
			newEntry.setSlot(shopSlot);
			newEntry.setShop(this);
			newEntry.saveToDatabase();
			addEntry(shopMode, shopSlot, newEntry);
		}
	}

	@Override
	public int compareTo(@NotNull Shop o) {
		return namePlain.compareTo(o.getNamePlain());
	}

	@Override
	@JsonIgnore
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createShopItemStack(this);
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveShop(this);
	}
}
