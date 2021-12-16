package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

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

	public EntryInteractionResult pay(Customer customer) {
		return pay(customer, 1.);
	}

	public EntryInteractionResult pay(Customer customer, double discount) {
		if (!canPay(customer)) {
			return EntryInteractionResult.FAIL_CANT_AFFORD;
		}
		currency.removeAmount(customer, amount * discount, object);
		return EntryInteractionResult.SUCCESS;
	}

	public EntryInteractionResult gain(Customer customer) {
		return gain(customer, 1.);
	}

	public EntryInteractionResult gain(Customer customer, double discount) {
		if (!canGain(customer)) {
			return EntryInteractionResult.FAIL_CANT_REWARD;
		}
		currency.addAmount(customer, amount * discount, object);
		return EntryInteractionResult.SUCCESS;
	}

	public Component getObjectComponent() {
		return currency.getCurrencyComponent(amount, object);
	}

	public Component getPriceComponent() {
		return getPriceComponent(1.);
	}

	public Component getPriceComponent(double discount) {
		return currency.format(amount, object, discount);
	}

	public boolean equals(Price<?> object) {
		if (this == object) {
			return true;
		}
		if (this.object instanceof ItemStack isa && object.object instanceof ItemStack isb) {
			return isa.isSimilar(isb);
		}
		return Objects.equals(this.object, object.object) && this.currency.equals(object.currency);
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
