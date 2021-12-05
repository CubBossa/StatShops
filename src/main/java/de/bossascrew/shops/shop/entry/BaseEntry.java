package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.shop.ShopMode;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

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

	@Override
	public UUID getUUID() {
		return uuid;
	}

	public boolean hasPermission(Customer customer) {
		return permission == null || customer.getPlayer().hasPermission(permission);
	}

	public ShopInteractionResult interact(Customer customer) {
		if (module == null) {
			return ShopInteractionResult.STATIC;
		}
		if (!hasPermission(customer)) {
			return ShopInteractionResult.FAIL_NO_PERMISSION;
		}
		ShopInteractionResult result = module.perform(customer);
		ShopPlugin.getInstance().getLogDatabase().logToDatabase(module.createLogEntry());
		return result;
	}

	public List<String> getTags() {
		List<String> tags = new ArrayList<>(this.tags);
		tags.add(uuid.toString());
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
		ShopPlugin.getInstance().getDatabase().saveEntry(this);
	}

	@Override
	public ShopEntry duplicate() {
		return new BaseEntry(UUID.randomUUID(), shop, displayItem.clone(), permission, slot, shopMode);
	}
}
