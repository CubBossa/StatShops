package de.bossascrew.shops.util;

import org.jetbrains.annotations.Nullable;

/**
 * Indicates if an object can be edited by only one editor.
 * It is used for example in shops, limits and discounts. When someone is editing a shop, no one can interact with the shop.
 *
 * @param <T> The type of the editor, in most cases probably Player
 */
public interface Editable<T> {

	/**
	 * @return the object of type T that is currently editing this shop. Only one object/player can edit a shop at a time. If null, no object/player is editing this shop.
	 */
	@Nullable T getEditor();

	/**
	 * @param editor sets the currently editing object of type T. If set to null, no one is editing this shop anymore.
	 */
	void setEditor(@Nullable T editor);
}
