package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import de.bossascrew.shops.statshops.shop.Transaction;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public interface TradeModule<T> extends EntryModule {

	void setCurrency(Currency<T> currency);

	@Nullable Currency<T> getCurrency();

	double getPriceAmount();

	void setPriceAmount(double amount);

	T getPriceObject();

	void setPriceObject(T object);

	void setPrice(double amount, T object);

	Component getPriceDisplay();

	ItemStack getArticle();

	boolean canGiveArticle(Customer customer);

	void giveArticle(Customer customer);

	Transaction getLastTransaction(Customer customer);
}
