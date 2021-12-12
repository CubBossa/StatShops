package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Config;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.util.TagUtils;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import de.bossascrew.shops.statshops.shop.ShopMode;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * A shop entry to be used for all itembased shops (not for chat shops or hologram shops e.g.)
 */
@Getter
@Setter
public class BaseEntry implements ShopEntry {

	private final UUID uuid;
	private Shop shop;
	private ItemStack displayItem;
	private @Nullable String permission;
	private String infoLoreFormat;
	private final List<String> tags;
	private @Nullable EntryModule module = null;

	private int slot;
	private ShopMode shopMode;

	public BaseEntry(UUID uuid, Shop shop, ItemStack displayItem, @Nullable String permission, int slot, ShopMode shopMode) {
		this.uuid = uuid;
		this.shop = shop;
		this.displayItem = displayItem;
		this.permission = permission;
		this.tags = new ArrayList<>();

		this.slot = slot;
		this.shopMode = shopMode;
	}

	public void setModule(@Nullable EntryModule module) {
		this.module = module;
		if (this.module != null) {
			this.module.setShopEntry(this);
		}
	}

	@Override
	public UUID getUUID() {
		return uuid;
	}

	public boolean hasPermission(Customer customer) {
		return permission == null || customer.getPlayer().hasPermission(permission);
	}

	@Override
	public <T> T getData(Class<T> clazz, String key) {
		return null;
	}

	@Override
	public <T> T storeData(Class<T> clazz, String key, T value) {
		return null;
	}

	public ShopInteractionResult interact(Customer customer) {
		if (module == null) {
			return ShopInteractionResult.STATIC;
		}
		if (!hasPermission(customer)) {
			return ShopInteractionResult.FAIL_NO_PERMISSION;
		}
		ShopInteractionResult result = module.perform(customer);
		StatShops.getInstance().getLogDatabase().logToDatabase(module.createLogEntry(customer, result));
		return result;
	}

	public List<String> getTags() {
		List<String> tags = shop.getTags().stream().map(s -> "(shop) " + s).collect(Collectors.toList());
		tags.add(uuid.toString());
		tags.addAll(this.tags);
		//If auto-tagging is enabled, add all material tags to the entry
		if (module != null && module instanceof TradeModule tm) {
			Config config = StatShops.getInstance().getShopsConfig();
			tags.addAll(TagUtils.getTags(tm.getArticle(),
					config.isAutoTaggingMaterials(),
					config.isAutoTaggingGroups(),
					config.isAutoTaggingEnchantments(),
					config.isAutoTaggingPotions(),
					config.isAutoTaggingAttributes()));
		}
		return tags;
	}

	@Override
	public boolean addTag(String tag) {
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
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveEntry(this);
	}

	@Override
	public ShopEntry duplicate() {
		BaseEntry entry = new BaseEntry(UUID.randomUUID(), shop, displayItem.clone(), permission, slot, shopMode);
		if (module != null) {
			EntryModule m = module.duplicate();
			entry.setModule(m);
		}
		return entry;
	}
}
