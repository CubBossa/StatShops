package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.api.ShopEntry;
import lombok.Getter;

public class ShopEntryEvent extends SimpleEvent {

	@Getter
	private final ShopEntry shopEntry;

	public ShopEntryEvent(ShopEntry shopEntry) {
		this.shopEntry = shopEntry;
	}
}
