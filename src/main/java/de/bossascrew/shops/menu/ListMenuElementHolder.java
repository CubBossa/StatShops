package de.bossascrew.shops.menu;

import java.util.List;

/**
 * Allows to be used as the list element holder for a ListMenu instance.
 * The element holder provides all elements to be listed in the menu
 *
 * @param <L> The element that will be listed in the ListMenu instance.
 */
public interface ListMenuElementHolder<L extends ListMenuElement> {

	/**
	 * @return The list of ListMenuElements to display in the menu
	 */
	List<L> getValues();
}
