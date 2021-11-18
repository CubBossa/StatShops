package de.bossascrew.shops.util;

/**
 * Can be duplicated. Instead of creating a clone of this object, duplicates will only be similar in their "visible attributes"
 * and not for example in attributes like UUID.
 *
 * @param <T> The result of the duplicate method
 */
public interface Duplicable<T> {

	/**
	 * @return a duplicate object with similar attributes except from technical attributes like UUIDs
	 */
	T duplicate();
}
