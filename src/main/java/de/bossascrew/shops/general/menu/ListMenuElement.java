package de.bossascrew.shops.general.menu;

import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

/**
 * Allows to be used as a listable object for the ListMenu class.
 */
public interface ListMenuElement {

	/**
	 * @return The displayname of the list element
	 */
	Component getName();

	/**
	 * @return The display icon of the list element
	 */
	ItemStack getListDisplayItem();
}
