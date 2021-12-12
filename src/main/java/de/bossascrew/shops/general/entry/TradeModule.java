package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.shop.Currency;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
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

	ItemStack getArticle();

	boolean canGiveArticle(Customer customer);

	void giveArticle(Customer customer);
}
