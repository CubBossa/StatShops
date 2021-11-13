package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
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
public class BaseShopEntry implements ShopEntry {

	private final UUID uuid;
	private Shop shop;
	private ItemStack displayItem;
	private EntryElement pay;
	private EntryElement gain;
	@Nullable
	private String permission;
	private final List<String> tags;

	private int slot;
	private ShopMode mode;

	public BaseShopEntry(UUID uuid, Shop shop, ItemStack displayItem, EntryElement pay, EntryElement gain, int slot, ShopMode shopMode) {
		this(uuid, shop, displayItem, pay, gain, null, slot, shopMode);
	}

	public BaseShopEntry(UUID uuid, Shop shop, ItemStack displayItem, EntryElement pay, EntryElement gain, @Nullable String permission, int slot, ShopMode shopMode) {
		this.uuid = uuid;
		this.shop = shop;
		this.displayItem = displayItem;
		this.pay = pay;
		this.gain = gain;
		this.permission = permission;
		this.tags = new ArrayList<>();

		this.slot = slot;
		this.mode = shopMode;
	}

	@Override
	public String getDisplayPrice() {
		return pay.getAmountDisplay();
	}

	public boolean hasPermission(Customer customer) {
		return permission == null || customer.getPlayer().hasPermission(permission);
	}

	public ShopInteractionResult buy(Customer customer) {
		return pay.act(customer).compare(gain.act(customer));
	}

	public List<String> getTags() {
		List<String> tags = new ArrayList<>(this.tags);
		tags.addAll(shop.getTags());
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
}
