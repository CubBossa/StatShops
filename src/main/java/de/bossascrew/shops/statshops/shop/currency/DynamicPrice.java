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
public class DynamicPrice<T> implements Price<T> {

	private final Currency<T> currency;
	private double amount;
	private String dynamicPriceString;
	private T object;

	public DynamicPrice(Currency<T> currency, String dynamicPriceString, T object) {
		this.currency = currency;
		this.dynamicPriceString = dynamicPriceString;
		this.object = object;
		this.amount = loadAmount();
	}

	public double loadAmount() {
		Double d = DynamicPricingHandler.getInstance().getPrice(dynamicPriceString);
		if (d == null) {
			return 10.;
		}
		return d;
	}

	@Override
	public void applyDiscount(double discount) {
		this.amount = this.currency.applyDiscount(amount, discount);
	}

	@Override
	public double getAmount(double discount) {
		return currency.applyDiscount(getAmount(), discount);
	}

	@Override
	public boolean canPay(Customer customer, double discount) {
		return currency.hasAmount(customer, currency.applyDiscount(amount, discount), object);
	}

	@Override
	public boolean canGain(Customer customer) {
		return true; //TODO
	}

	@Override
	public boolean canGain(Customer customer, double discount) {
		return true;
	}

	@Override
	public EntryInteractionResult pay(Customer customer) {
		return pay(customer, 1.);
	}

	@Override
	public EntryInteractionResult pay(Customer customer, double discount) {
		if (!canPay(customer, discount)) {
			return EntryInteractionResult.FAIL_CANT_AFFORD;
		}
		currency.removeAmount(customer, currency.applyDiscount(amount, discount), object);
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public EntryInteractionResult gain(Customer customer) {
		return gain(customer, 1.);
	}

	@Override
	public EntryInteractionResult gain(Customer customer, double discount) {
		if (!canGain(customer)) {
			return EntryInteractionResult.FAIL_CANT_REWARD;
		}
		currency.addAmount(customer, currency.applyDiscount(amount, discount), object);
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public Component getObjectComponent() {
		return currency.getCurrencyComponent(amount, object);
	}

	@Override
	public Component getPriceComponent() {
		return getPriceComponent(1.);
	}

	@Override
	public Component getPriceComponent(double discount) {
		return currency.format(amount, object, discount);
	}

	@Override
	public boolean equals(Price<?> object) {
		if (this == object) {
			return true;
		}
		if (this.object instanceof ItemStack isa && object.getObject() instanceof ItemStack isb) {
			return isa.isSimilar(isb);
		}
		return Objects.equals(this.object, object.getObject()) && this.currency.equals(object.getCurrency());
	}

	@Override
	public Price<T> duplicate() {
		return new DynamicPrice<>(currency, dynamicPriceString, object);
	}

	@Override
	public int compareTo(@NotNull Price<T> o) {
		return Double.compare(amount, o.getAmount());
	}

	public SimplePrice<T> toSimplePrice() {
		return new SimplePrice<>(currency, amount, object);
	}
}
