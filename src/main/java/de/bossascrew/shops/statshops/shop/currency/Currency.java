package de.bossascrew.shops.statshops.shop.currency;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Customer;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.jetbrains.annotations.Nullable;

import java.util.function.BiFunction;
import java.util.function.Function;

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
@Setter
public abstract class Currency<T> {

	private final String key;
	private final String format;
	private final String formatDiscounted;

	private double lowerBounds = Integer.MIN_VALUE;
	private double threshold = 0;
	private double upperBounds = Integer.MAX_VALUE;

	private final Function<Double, String> countFormatter;
	private final BiFunction<Double, T, Component> currencyFormatter;

	/**
	 * @param key               An individual key to recognize the currency
	 * @param format            the way to display the currency in minimessage format. Valid placeholders are "amount" and "currency"
	 * @param currencyFormatter It provides the component for the currency. If the currency is itemstack, for example, the function could return
	 */
	public Currency(String key, String format, String formatDiscounted, BiFunction<Double, T, Component> currencyFormatter) {
		this(key, format, formatDiscounted, d -> String.format("%.2f", d), currencyFormatter);
	}

	/**
	 * @param key               An individual key to recognize the currency
	 * @param format            the way to display the currency in minimessage format. Valid placeholders are "amount" and "currency"
	 * @param countFormatter    Formats the given amount to a readable string.
	 * @param currencyFormatter It provides the component for the currency. If the currency is itemstack, for example, the function could return
	 */
	public Currency(String key, String format, String formatDiscounted, Function<Double, String> countFormatter, BiFunction<Double, T, Component> currencyFormatter) {
		this.key = key;
		this.format = format;
		this.formatDiscounted = formatDiscounted;
		this.countFormatter = countFormatter;
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
	public Component format(double amount, @Nullable T object, double discount) {
		return StatShops.getInstance().getMiniMessage().deserialize(discount != 1 ? formatDiscounted : format,
				TagResolver.resolver("amount", Tag.inserting(Component.text(countFormatter.apply(amount)))),
				TagResolver.resolver("amount_dc", Tag.inserting(Component.text(countFormatter.apply(applyDiscount(amount, discount))))),
				TagResolver.resolver("currency", Tag.inserting(currencyFormatter.apply(amount, object))));
	}

	public Component getCurrencyComponent(double amount, T object) {
		return currencyFormatter.apply(amount, object);
	}

	public boolean hasAmount(Customer customer, double amount, T object) {
		return getAmount(customer, object) - threshold >= amount;
	}

	public abstract double applyDiscount(double amount, double discount);

	public abstract double getAmount(Customer customer, T object);

	public abstract boolean addAmount(Customer customer, double amount, T object);

	public abstract boolean removeAmount(Customer customer, double amount, T object);
}
