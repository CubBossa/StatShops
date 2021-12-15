package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.shop.Discount;

public class DiscountExpireEvent extends DiscountEvent {

	public DiscountExpireEvent(Discount discount) {
		super(discount);
	}
}
