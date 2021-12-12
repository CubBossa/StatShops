package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.StatShops;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import org.jetbrains.annotations.Nullable;

import java.util.function.BiFunction;

/**
 * Manages the currency of a shopentry. Example currencies could be dollars, votegems, items, entities
 *
 * @param <T> The type of the currency object. In case of a virtual currency like votegems and dollars, T can simply be of type Void.
 *            When wanting to trade for emeralds with a certain NBT Tag, use the ItemStack class for T and check if the item is of type EMERALD and
 *            has the required NBT Type in the getAmount() method.
 *            If you want to trade any item and only validate by its type, you can use Material as T and iterate the players inventory in getAmount() and check for items with
 *            the provided type.
 *            If you want to trade entities, for example, you could use the EntityType as T. In case of entities, the getAmount() method could check for leashed
 *            entities of passengers.
 */
@Getter
public abstract class Currency<T> {

	private final String format;
	private final BiFunction<Double, T, Component> currencyFormatter;

	/**
	 * @param format            the way to display the currency in minimessage format. Valid placeholders are "amount" and "currency"
	 * @param currencyFormatter It provides the component for the currency. If the currency is itemstack, for example, the function could return
	 *                          the translatable component of the material. It accepts the amount to allow singular and plural currencies (1 Dollar, 2 Dollars)
	 */
	public Currency(String format, BiFunction<Double, T, Component> currencyFormatter) {
		this.format = format;
		this.currencyFormatter = currencyFormatter;
	}

	/**
	 * Returns the formatted price component for this currency.
	 *
	 * @param amount The amount of the price. It might be, that the formatted version does not contain any amount at all. Or that the
	 *               currency does not even support multiple amounts. The currency could, for example, just be a required key card and
	 *               the formatted component would simply be "Key Card Required".
	 * @param object An instance of the currency type. In many cases (like vault money) the object can simply be null.
	 *               But if T, for example, is an ItemStack, then the returned could be the translational component for
	 *               "1x Diamond" or "3x Enchanted Sword", depending on the provided currencyFormatter.
	 * @return a component that displays the amount of T objects in a user-friendly way
	 */
	public Component format(double amount, @Nullable T object) {
		return StatShops.getInstance().getMiniMessage().parse(format,
				Template.of("amount", "" + amount),
				Template.of("currency", currencyFormatter.apply(amount, object)));
	}

	public boolean hasAmount(Customer customer, double amount, T object) {
		return getAmount(customer, object) >= amount;
	}

	public abstract double getAmount(Customer customer, T object);

	public abstract boolean addAmount(Customer customer, double amount, T object);

	public abstract boolean removeAmount(Customer customer, double amount, T object);
}
