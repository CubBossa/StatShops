package de.bossascrew.shops.data;

/**
 * Can and needs to be saved to be restored after restart.
 */
public interface DatabaseObject {

	/**
	 * saves this object and its attributes into the provided database
	 */
	void saveToDatabase();
}
