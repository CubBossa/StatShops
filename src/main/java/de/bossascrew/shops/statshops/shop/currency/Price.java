package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.handler.CurrencyHandler;
import de.bossascrew.shops.statshops.handler.DynamicPricingHandler;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

@Getter
public class Price<T> implements Comparable<Price<?>>, ConfigurationSerializable {

	private final Currency<T> currency;
	@Setter
	private double amount;
	private String dynamicPriceString;
	@Setter
	private T object;

	public Price(Currency<T> currency, double amount, T object) {
		this.currency = currency;
		this.object = object;
		this.amount = amount;
		this.dynamicPriceString = null;
	}

	public Price(Currency<T> currency, String dynamicPriceString, double fallback, T object) {
		this.currency = currency;
		this.object = object;
		setDynamicPriceString(dynamicPriceString, fallback);
	}

	/**
	 * deserialize constructor
	 */
	public Price(Map<String, Object> values) throws ClassCastException {
		this.amount = (double) values.get("amount");
		this.currency = (Currency<T>) CurrencyHandler.getInstance().getCurrency((String) values.get("currency"));
		var val = values.get("object");
		this.object = val == null ? null : (T) val;
	}


	public void setDynamicPriceString(String dynamicPriceString, double fallback) {
		this.dynamicPriceString = dynamicPriceString;
		bakeDynamicPricing(null, fallback);
	}

	public void bakeDynamicPricing(@Nullable Customer customer, double fallback) {
		this.amount = loadAmount(null, fallback);
	}

	public double loadAmount(@Nullable Customer customer, double fallback) {
		if (this.dynamicPriceString == null) {
			return amount;
		}
		Double d = DynamicPricingHandler.getInstance().getPrice(customer, dynamicPriceString);
		if (d == null) {
			return fallback;
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
		return canGain(customer, 1.);
	}


	public boolean canGain(Customer customer, double discount) {
		return currency.getAmount(customer, object) + currency.applyDiscount(amount, discount) <= currency.getUpperBounds();
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


	public boolean summable(Price<?> object) {
		return compareEquals(object, false);
	}

	public boolean equals(Price<?> object) {
		return compareEquals(object, true);
	}

	private boolean compareEquals(Price<?> object, boolean considerAmount) {
		if (this == object) {
			return true;
		}
		if(considerAmount && this.amount != object.amount) {
			return false;
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
		return new Price<>(currency, dynamicPriceString, amount, object);
	}

	public Price<T> toSimplePrice(Customer customer) {
		if (dynamicPriceString != null) {
			bakeDynamicPricing(customer, amount);
		}
		return new Price<>(currency, amount, object);
	}

	@Override
	public int compareTo(@NotNull Price<?> o) {
		return Double.compare(amount, o.getAmount());
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		HashMap<String, Object> map = new HashMap<>();
		map.put("currency", currency.getKey());
		map.put("amount", amount);
		map.put("object", object);
		return map;
	}
}
