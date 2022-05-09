package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.shop.entry.BaseEntry;
import de.cubbossa.menuframework.inventory.Menu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

@Getter
@Setter
public abstract class BaseShop implements Shop {

	protected final UUID uuid;
	protected String nameFormat;
	protected Component name;
	protected String namePlain;

	protected ItemStack displayItem;
	protected @Nullable String permission = null;

	protected final Map<UUID, ShopEntry> uuidEntryMap;
	protected final TreeMap<Integer, ShopEntry> entryMap;
	/**
	 * contains all entries that were removed via editor. When later on an item with the uuid tag of this entry was added, it can be restored from cache.
	 */
	protected final Map<UUID, ShopEntry> unusedEntryCache;

	protected final List<Customer> activeCustomers;
	protected final Map<Customer, ShopMenu> menuMap;
	protected final List<String> tags;

	protected @Nullable Player editor = null;

	public BaseShop(UUID uuid, String nameFormat) {
		this.uuid = uuid;
		setNameFormat(nameFormat);

		this.uuidEntryMap = new HashMap<>();
		this.entryMap = new TreeMap<>();
		this.unusedEntryCache = new HashMap<>();
		this.activeCustomers = new ArrayList<>();
		this.menuMap = new HashMap<>();
		this.tags = new ArrayList<>();
	}

	@Override
	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().deserialize(nameFormat);
		this.namePlain = TextUtils.toPlain(this.name);
	}

	@Override
	public UUID getUUID() {
		return uuid;
	}

	public void setPermission(String permission) {
		this.permission = permission != null ? permission.equalsIgnoreCase("null") ? null : permission : null;
	}

	@Override
	public SortedMap<Integer, ShopEntry> getEntries() {
		return entryMap;
	}

	public @Nullable
	ShopEntry getEntry(int slot) {
		return entryMap.getOrDefault(slot, null);
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
		if (getEntry(entry.getUUID()) != null) {
			moveEntry(entry, slot);
			return entry;
		}
		ShopEntry oldEntry = entryMap.put(slot, entry);
		if (oldEntry != null) {
			StatShops.getInstance().getDatabase().deleteEntry(oldEntry);
		}
		uuidEntryMap.put(entry.getUUID(), entry);
		unusedEntryCache.remove(entry.getUUID());

		entry.setSlot(slot);
		StatShops.getInstance().getDatabase().saveEntry(entry);
		return entry;
	}

	@Override
	public boolean moveEntry(ShopEntry entry, int slot) {
		if (slot == entry.getSlot()) {
			return true;
		}
		if (getEntry(entry.getUUID()) == null) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Tried to move an entry that was not contained in this shop.");
			return false;
		}
		boolean override = false;
		ShopEntry oldEntry = getEntry(slot);
		if (oldEntry != null && !oldEntry.equals(entry)) {
			setEntryUnused(oldEntry);
			override = true;
		}
		entryMap.remove(entry.getSlot());
		entry.setSlot(slot);
		entry.saveToDatabase();
		unusedEntryCache.remove(entry.getUUID());
		entryMap.put(slot, entry);
		return override;
	}

	@Override
	public boolean swapEntries(int slot, int other) {
		if(slot == other) {
			return true;
		}
		ShopEntry a = entryMap.get(slot);
		ShopEntry b = entryMap.get(other);

		if(a != null) {
			a.setSlot(other);
			a.saveToDatabase();
			entryMap.put(other, a);
		} else {
			entryMap.remove(other);
		}
		if(b != null) {
			b.setSlot(slot);
			b.saveToDatabase();
			entryMap.put(slot, b);
		} else {
			entryMap.remove(slot);
		}

		return b != null;
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
	public Collection<ShopEntry> getUnusedEntries() {
		return unusedEntryCache.values();
	}

	@Override
	public void addUnusedEntry(ShopEntry entry) {
		uuidEntryMap.put(entry.getUUID(), entry);
		unusedEntryCache.put(entry.getUUID(), entry);
	}

	@Override
	public boolean setEntryUnused(ShopEntry entry) {
		boolean success = entryMap.remove(entry.getSlot(), entry);
		unusedEntryCache.put(entry.getUUID(), entry);
		entry.setSlot(-1);
		entry.saveToDatabase();
		return success;
	}

	@Override
	public int cleanupUnusedEntries() {
		for (ShopEntry shopEntry : getUnusedEntries()) {
			StatShops.getInstance().getDatabase().deleteEntry(shopEntry);
		}
		int count = unusedEntryCache.size();
		unusedEntryCache.clear();
		return count;
	}

	@Override
	public List<Customer> getActiveCustomers() {
		return null;
	}

	@Override
	public boolean open(Customer customer) {
		newShopMenu(customer).open(customer.getPlayer());
		return true;
	}

	@Override
	public void closeAll() {
		for (Customer customer : activeCustomers) {
			close(customer);
		}
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
	public int compareTo(@NotNull Shop o) {
		return namePlain.compareTo(o.getNamePlain());
	}

	public ItemStack getDisplayItem() {
		return displayItem == null ? null : displayItem.clone();
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().runAsync(() -> StatShops.getInstance().getDatabase().saveShop(this));
	}
}
