package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.shop.Taggable;
import org.bukkit.inventory.ItemStack;

public interface ShopEntry extends Taggable {

	int getSlot();

	Shop getShop();

	ItemStack getDisplayItem();

	String getDisplayPrice();

	boolean hasPermission(Customer customer);

	ShopInteractionResult buy(Customer customer);
}
