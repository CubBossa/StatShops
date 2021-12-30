package de.bossascrew.shops.statshops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.VillagerMenu;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.menu.VillagerShopEditor;
import de.bossascrew.shops.statshops.menu.VillagerShopMenu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

@Getter
@Setter
public class VillagerShop implements Shop {

	private final UUID uuid;
	private String nameFormat;

	@JsonIgnore
	private Component name;
	@JsonIgnore
	private String namePlain;

	private boolean enabled = true;

	@JsonIgnore
	private Material displayMaterial;
	private @Nullable String permission = null;
	private @Nullable EntryTemplate defaultTemplate = null;

	@JsonIgnore
	private @Nullable Player editor = null;

	private final Map<Integer, ShopEntry> slotEntryMap;
	@JsonIgnore
	private final Map<UUID, ShopEntry> uuidEntryMap;
	/**
	 * contains all entries that were removed via editor. When later on an item with the uuid tag of this entry was added, it can be restored from cache.
	 */
	@JsonIgnore
	private final Map<UUID, ShopEntry> unusedEntryCache;

	@JsonIgnore
	private final List<Customer> activeCustomers;

	private final Map<Customer, VillagerMenu> menuMap;

	private final List<String> tags;

	public VillagerShop(String nameFormat) {
		this(nameFormat, UUID.randomUUID());
	}

	public VillagerShop(String nameFormat, UUID uuid) {
		setNameFormat(nameFormat);
		this.uuid = uuid;

		this.slotEntryMap = new TreeMap<>();
		this.uuidEntryMap = new HashMap<>();
		this.unusedEntryCache = new HashMap<>();
		this.activeCustomers = new ArrayList<>();
		this.menuMap = new HashMap<>();
		this.tags = new ArrayList<>();
	}

	@Override
	public UUID getUUID() {
		return null;
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = TextUtils.toPlain(this.name);
	}

	@Override
	public ShopEntry createEntry(ItemStack displayItem, int slot) {
		return null;
	}

	@Override
	public ShopEntry addEntry(ShopEntry entry, int slot) {
		slotEntryMap.put(slot, entry);
		uuidEntryMap.put(entry.getUUID(), entry);
		return entry;
	}

	@Override
	public boolean moveEntry(ShopEntry entry, int slot) {
		return false;
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
	public boolean deleteEntry(ShopEntry entry) {
		if (entry == null) {
			return false;
		}
		uuidEntryMap.remove(entry.getUUID());
		StatShops.getInstance().getDatabase().deleteEntry(entry);
		return slotEntryMap.remove(entry.getSlot(), entry);
	}

	@Override
	public boolean removeEntry(ShopEntry entry) {
		return false;
	}

	@Override
	public boolean deleteEntry(int slot) {
		return false;
	}

	@Override
	public boolean setEntryUnused(ShopEntry entry) {
		unusedEntryCache.put(entry.getUUID(), entry);
		return slotEntryMap.remove(entry.getSlot(), entry);
	}

	@Override
	public void applyTemplate(EntryTemplate template) {

	}

	@Override
	public boolean open(Customer customer) {
		return open(customer, null);
	}

	@Override
	public boolean open(Customer customer, @Nullable ContextConsumer<CloseContext> closeHandler) {
		return new VillagerShopMenu(this, customer).openInventory(customer.getPlayer()) != null;
	}

	public boolean close(Customer customer) {
		VillagerMenu menu = menuMap.get(customer);
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
		new VillagerShopEditor(this, backHandler).openInventory(player);
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveShop(this);
	}

	@Override
	@JsonIgnore
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createShopItemStack(this);
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
	public int compareTo(@NotNull Shop o) {
		return namePlain.compareTo(o.getNamePlain());
	}
}
