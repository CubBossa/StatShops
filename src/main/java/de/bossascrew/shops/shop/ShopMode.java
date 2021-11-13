package de.bossascrew.shops.shop;

import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

/**
 * Buy, Sell, Trade, etc...
 */
@Getter
@Setter
public abstract class ShopMode {

	private ShopMode next = null;
	private ShopMode previous = null;

	public abstract String getKey();

	public abstract Component getDisplayName();

	public abstract ItemStack getDisplayItem();
}
