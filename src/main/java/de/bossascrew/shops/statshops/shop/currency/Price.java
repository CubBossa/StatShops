package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Price<T> {

	private final Currency<T> currency;
	private double amount;
	private final T object;

	public Price(Currency<T> currency, double amount, T object) {
		this.currency = currency;
		this.amount = amount;
		this.object = object;
	}

	public ShopInteractionResult pay(Customer customer) {
		if(currency.hasAmount(customer, amount, object)) {
			return ShopInteractionResult.SUCCESS;
		}
		return ShopInteractionResult.FAIL_CANT_AFFORD;
	}

	public ShopInteractionResult gain(Customer customer) {
		currency.addAmount(customer, amount, object);
		return ShopInteractionResult.SUCCESS;
	}

	public boolean equals(Price<?> object) {
		if(this == object) {
			return true;
		}
		return this.object.equals(object.object) && this.currency.equals(object.currency);
	}
}
