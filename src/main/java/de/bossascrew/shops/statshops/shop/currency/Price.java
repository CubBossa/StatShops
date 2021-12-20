package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import net.kyori.adventure.text.Component;

public interface Price<T> extends Duplicable<Price<T>>, Comparable<Price<T>> {

	void applyDiscount(double discount);

	double getAmount(double discount);

	boolean canPay(Customer customer, double discount);

	boolean canGain(Customer customer);

	boolean canGain(Customer customer, double discount);

	EntryInteractionResult pay(Customer customer);

	EntryInteractionResult pay(Customer customer, double discount);

	EntryInteractionResult gain(Customer customer);

	EntryInteractionResult gain(Customer customer, double discount);

	Component getObjectComponent();

	Component getPriceComponent();

	Component getPriceComponent(double discount);

	boolean equals(Price<?> object);

	Currency<T> getCurrency();

	double getAmount();

	T getObject();

	void setAmount(double amount);

	void setObject(T object);
}
