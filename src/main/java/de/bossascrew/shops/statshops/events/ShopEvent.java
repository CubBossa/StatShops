package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.general.Shop;
import lombok.Getter;

public class ShopEvent extends SimpleEvent {

	@Getter
	private final Shop shop;

	public ShopEvent(Shop shop) {
		this.shop = shop;
	}
}
