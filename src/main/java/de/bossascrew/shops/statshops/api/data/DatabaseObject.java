package de.bossascrew.shops.statshops.api.data;

/**
 * Object that can be stored into a database.
 */
public interface DatabaseObject {

	/**
	 * Saves this object to the default database provided by the database handler
	 */
	void saveToDatabase();
}
