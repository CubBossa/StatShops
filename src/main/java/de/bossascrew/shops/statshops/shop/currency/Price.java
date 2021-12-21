package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.handler.DynamicPricingHandler;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

@Getter
@Setter
public class Price<T> implements Comparable<Price<?>> {

	private final Currency<T> currency;
	private double amount;
	private String dynamicPriceString;
	private T object;

	public Price(Currency<T> currency, double amount, T object) {
		this.currency = currency;
		this.object = object;
		this.amount = amount;
		this.dynamicPriceString = null;
	}

	public Price(Currency<T> currency, String dynamicPriceString, T object) {
		this.currency = currency;
		this.object = object;
		setDynamicPriceString(dynamicPriceString);
	}

	public void setDynamicPriceString(String dynamicPriceString) {
		this.dynamicPriceString = dynamicPriceString;
		bakeDynamicPricing();
	}

	public void bakeDynamicPricing() {
		this.amount = loadAmount();
		System.out.println(amount);
	}

	public double loadAmount() {
		if (this.dynamicPriceString == null) {
			return amount;
		}
		Double d = DynamicPricingHandler.getInstance().getPrice(dynamicPriceString);
		if (d == null) {
			return 10.;
		}
		return d;
	}


	public void applyDiscount(double discount) {
		this.amount = this.currency.applyDiscount(amount, discount);
	}


	public double getAmount(double discount) {
		return currency.applyDiscount(getAmount(), discount);
	}


	public boolean canPay(Customer customer, double discount) {
		return currency.hasAmount(customer, currency.applyDiscount(amount, discount), object);
	}


	public boolean canGain(Customer customer) {
		return true; //TODO in currency ein lower limit, ein upper limit und eine transaction schwelle (0$ zb) definieren
	}


	public boolean canGain(Customer customer, double discount) {
		return true;
	}


	public EntryInteractionResult pay(Customer customer) {
		return pay(customer, 1.);
	}


	public EntryInteractionResult pay(Customer customer, double discount) {
		if (!canPay(customer, discount)) {
			return EntryInteractionResult.FAIL_CANT_AFFORD;
		}
		currency.removeAmount(customer, currency.applyDiscount(amount, discount), object);
		return EntryInteractionResult.SUCCESS;
	}


	public EntryInteractionResult gain(Customer customer) {
		return gain(customer, 1.);
	}


	public EntryInteractionResult gain(Customer customer, double discount) {
		if (!canGain(customer)) {
			return EntryInteractionResult.FAIL_CANT_REWARD;
		}
		currency.addAmount(customer, currency.applyDiscount(amount, discount), object);
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
		if (this.object != null && this.object instanceof ItemStack isa && object.getObject() instanceof ItemStack isb) {
			if (isa.isSimilar(isb)) {
				if (this.currency.equals(object.getCurrency())) {
					return true;
				}
			}
		}
		return Objects.equals(this.object, object.getObject()) && this.currency.equals(object.getCurrency());
	}


	public Price<T> duplicate() {
		if (dynamicPriceString == null) {
			return new Price<>(currency, amount, object);
		}
		return new Price<>(currency, dynamicPriceString, object);
	}

	public Price<T> toSimplePrice() {
		if (dynamicPriceString != null) {
			bakeDynamicPricing();
		}
		return new Price<>(currency, amount, object);
	}

	@Override
	public int compareTo(@NotNull Price<?> o) {
		return Double.compare(amount, o.getAmount());
	}
}
