package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.general.entry.ShopEntry;

import java.time.Duration;

public interface ShopMenu {

	void updateEntry(ShopEntry shopEntry);

	void handleLimitRecoverInit(ShopEntry shopEntry, long recoverDuration);
}
