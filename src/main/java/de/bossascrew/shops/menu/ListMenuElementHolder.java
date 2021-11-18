package de.bossascrew.shops.menu;

import java.util.List;

/**
 * Allows to be used as the list element holder for a ListMenu instance.
 * The element holder provides all elements to be listed in the menu und handles
 * the interaction results create, delete and duplicate
 * @param <L> The element that will be listed in the ListMenu instance.
 */
public interface ListMenuElementHolder<L extends ListMenuElement> {

	/**
	 * @return The list of ListMenuElements to display in the menu
	 */
	List<L> getValues();

	/**
	 * @param input the input string from the anvil gui
	 * @return A new object with default values
	 */
	L createNew(String input);

	/**
	 * @param element the input element to create a duplicate of
	 * @return a new element with similar values to the input element except from the objects UUID
	 */
	L createDuplicate(L element);

	/**
	 * @param element the input element to delete
	 * @return true, if the input element existed in the storage and was successfully removed
	 */
	boolean delete(L element);
}
