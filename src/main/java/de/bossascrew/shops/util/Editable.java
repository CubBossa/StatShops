package de.bossascrew.shops.util;

import org.jetbrains.annotations.Nullable;

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
