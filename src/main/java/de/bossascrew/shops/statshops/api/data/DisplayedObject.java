package de.bossascrew.shops.statshops.api.data;

import org.bukkit.inventory.ItemStack;

/**
 * Objects that can be displayed via GUI. This might be used in combination with {@link NamedObject} to modify
 * the display name before returning.
 */
public interface DisplayedObject {

	/**
	 * @return a clone of the display item. Modifying this {@link ItemStack} will not change the actual display item.
	 */
	ItemStack getDisplayItem();

	/**
	 * Sets the display item instance. The stack must be cloned, so later changes on the {@link ItemStack} don't
	 * accidentally affect the display item.
	 *
	 * @param stack a reference instance of {@link ItemStack} for the new display item.
	 */
	void setDisplayItem(ItemStack stack);
}
