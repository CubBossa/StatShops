package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.shop.Discount;
import lombok.Getter;

public class DiscountEvent extends SimpleEvent {

	@Getter
	private final Discount discount;

	public DiscountEvent(Discount discount) {
		this.discount = discount;
	}
}
