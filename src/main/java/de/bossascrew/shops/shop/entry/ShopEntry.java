package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.Taggable;
import de.bossascrew.shops.util.Duplicable;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public interface ShopEntry extends Taggable, Duplicable<ShopEntry>, DatabaseObject {

	UUID getUUID();

	Shop getShop();

	void setShop(Shop shop);

	int getSlot();

	void setSlot(int slot);

	ShopMode getShopMode();

	void setShopMode(ShopMode shopMode);

	ItemStack getDisplayItem();

	String getDisplayPrice();

	String getInfoLoreFormat();

	void setInfoLoreFormat(String lore);

	@Nullable String getPermission();

	void setPermission(@Nullable String permission);

	boolean hasPermission(Customer customer);

	ShopInteractionResult buy(Customer customer);
}
