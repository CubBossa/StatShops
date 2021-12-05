package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.Currency;
import de.bossascrew.shops.util.ComponentUtils;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Nullable;

public interface TradeModule<T> extends EntryModule {

	void setCurrency(Currency<T> currency);

	@Nullable Currency<T> getCurrency();

	double getPriceAmount();

	void setPriceAmount(double amount);

	T getPriceObject();

	Component getPriceDisplay();

	void setPriceObject(T object);

	void setPrice(double amount, T object);

	void giveArticle(Customer customer);
}
