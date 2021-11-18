package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.Taggable;
import de.bossascrew.shops.util.Duplicable;
import org.bukkit.inventory.ItemStack;

public interface ShopEntry extends Taggable, Duplicable<ShopEntry>, DatabaseObject {

	int getSlot();

	void setSlot(int slot);

	ShopMode getShopMode();

	void setShopMode(ShopMode shopMode);

	Shop getShop();

	ItemStack getDisplayItem();

	String getDisplayPrice();

	boolean hasPermission(Customer customer);

	ShopInteractionResult buy(Customer customer);
}
