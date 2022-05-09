package de.bossascrew.shops.statshops.api;

import de.cubbossa.menuframework.inventory.Menu;

/**
 * The menu that will be opened by an according shop. This does not necessarily have to be a GUI.
 * Images on maps, although not easy to set up, could also serve as ShopMenus.
 */
public interface ShopMenu extends Menu {

	/**
	 * Updates a {@link ShopEntry} and resets its display item with custom name and lore.
	 * This can be used to update discount changes, limit changes, dynamic pricing changes or,
	 * if supported, placeholderAPI changes.
	 *
	 * @param shopEntry the {@link ShopEntry} to replace in the shop menu.
	 */
	void updateEntry(ShopEntry shopEntry);

	/**
	 * Informs the {@link ShopMenu} about an interaction with a limited item.
	 * With this information, the shop entry can start a scheduler to refresh the {@link ShopEntry}
	 * once the limit has recovered. This is important especially for short limit recoveries (30s, 5min), which might
	 * even recover while the menu is open.
	 *
	 * @param shopEntry       the {@link ShopEntry} that was clicked and has to be refreshed later on.
	 * @param recoverDuration the duration in milliseconds to wait before refreshing the {@link ShopEntry}
	 */
	void handleLimitRecoverInit(ShopEntry shopEntry, long recoverDuration);
}
