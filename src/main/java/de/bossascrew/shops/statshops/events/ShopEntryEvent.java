package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.general.entry.ShopEntry;
import lombok.Getter;

public class ShopEntryEvent extends SimpleEvent {

	@Getter
	private final ShopEntry shopEntry;

	public ShopEntryEvent(ShopEntry shopEntry) {
		this.shopEntry = shopEntry;
	}
}
