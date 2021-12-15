package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.Transaction;
import lombok.Getter;

public class DiscountAppliedEvent extends DiscountEvent {

	@Getter
	private final Transaction transaction;

	public DiscountAppliedEvent(Discount discount, Transaction transaction) {
		super(discount);
		this.transaction = transaction;
	}
}
