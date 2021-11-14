package de.bossascrew.shops.web;

import java.util.List;

/**
 * @param <T> the cached objects, like shops, discounts and limits
 */
public interface WebAccessable<T> {

	/**
	 * @return a sorted list of the cached objects
	 */
	List<T> getWebData();

	/**
	 * @param values a list of all changed values of the webinterface that need to be updated in cache and database
	 */
	void storeWebData(List<T> values);

}
