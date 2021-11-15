package de.bossascrew.shops.shop;

import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

import java.util.Objects;

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

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (!(o instanceof ShopMode)) {
			return false;
		}
		ShopMode mode = (ShopMode) o;
		return getKey().equalsIgnoreCase(mode.getKey());
	}
}
