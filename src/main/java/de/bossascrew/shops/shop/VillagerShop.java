package de.bossascrew.shops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.menu.VillagerMenu;
import de.bossascrew.shops.menu.VillagerShopEditor;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.shop.entry.TradeModule;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.MerchantRecipe;
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
		this.name = ShopPlugin.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = ComponentUtils.toPlain(this.name);
	}

	@Override
	public void newEntry(int slot, ShopEntry entry) {
		slotEntryMap.put(slot, entry);
		uuidEntryMap.put(entry.getUUID(), entry);
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
		ShopPlugin.getInstance().getDatabase().deleteEntry(entry);
		return slotEntryMap.remove(entry.getSlot(), entry);
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
	public boolean open(Customer customer, @Nullable ContextConsumer<BackContext> backHandler) {
		VillagerMenu villagerMenu = new VillagerMenu(name, null);
		for (Map.Entry<Integer, ShopEntry> entry : slotEntryMap.entrySet()) {

			ShopEntry e = entry.getValue();
			//TODO sehr experimentell
			if (e.getModule() != null && e.getModule() instanceof TradeModule tm) {
				Currency<ItemStack> currency = tm.getCurrency();
				ItemStack price = (ItemStack) tm.getPriceObject();
				price.setAmount(Integer.min((int) tm.getPriceAmount(), 64));

				MerchantRecipe recipe = new MerchantRecipe(tm.getArticle(), Integer.MAX_VALUE); //TODO limits einpflegen
				recipe.addIngredient(price);
				double discount = DiscountHandler.getInstance().combineDiscounts(e, e.getShop());
				recipe.setPriceMultiplier((float) discount);

				villagerMenu.setMerchantOffer(entry.getKey(), recipe);
			}
		}
		return villagerMenu.openInventory(customer.getPlayer()) != null;
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
		ShopPlugin.getInstance().getDatabase().saveShop(this);
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
