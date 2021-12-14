package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

@Getter
@Setter
public class Price<T> implements Duplicable<Price<T>>, Comparable<Price<T>> {

	private final Currency<T> currency;
	private double amount;
	private final T object;

	public Price(Currency<T> currency, double amount, T object) {
		this.currency = currency;
		this.amount = amount;
		this.object = object;
	}

	public boolean canPay(Customer customer) {
		return currency.hasAmount(customer, amount, object);
	}

	public boolean canGain(Customer customer) {
		return true; //TODO
	}

	public ShopInteractionResult pay(Customer customer) {
		if (!canPay(customer)) {
			return ShopInteractionResult.FAIL_CANT_AFFORD;
		}
		currency.removeAmount(customer, amount, object);
		return ShopInteractionResult.SUCCESS;
	}

	public ShopInteractionResult gain(Customer customer) {
		if (!canGain(customer)) {
			return ShopInteractionResult.FAIL_CANT_REWARD;
		}
		currency.addAmount(customer, amount, object);
		return ShopInteractionResult.SUCCESS;
	}

	public Component getObjectComponent() {
		return currency.getCurrencyComponent(amount, object);
	}

	public Component getPriceComponent() {
		return currency.format(amount, object);
	}

	public boolean equals(Price<?> object) {
		if (this == object) {
			return true;
		}
		if (this.object instanceof ItemStack isa && object.object instanceof ItemStack isb) {
			return isa.isSimilar(isb);
		}
		return this.object.equals(object.object) && this.currency.equals(object.currency);
	}

	@Override
	public Price<T> duplicate() {
		return new Price<>(currency, amount, object);
	}

	@Override
	public int compareTo(@NotNull Price<T> o) {
		return Double.compare(amount, o.amount);
	}
}
