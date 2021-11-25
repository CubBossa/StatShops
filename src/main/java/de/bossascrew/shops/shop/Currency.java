package de.bossascrew.shops.shop;

import de.bossascrew.shops.ShopPlugin;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import org.jetbrains.annotations.Nullable;

import java.util.function.BiFunction;

@Getter
public class Currency<T> {

	private final String format;
	private final BiFunction<Integer, T, Component> currencyFormatter;

	/**
	 * @param format            the way to display the currency in minimessage format. Valid placeholders are "amount" and "currency"
	 * @param currencyFormatter It provides the component for the currency. If the currency is itemstack, for example, the function could return
	 *                          the translatable component of the material. It accepts the amount to allow singular and plural currencies (1 Dollar, 2 Dollars)
	 */
	public Currency(String format, BiFunction<Integer, T, Component> currencyFormatter) {
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
	public Component format(int amount, @Nullable T object) {
		return ShopPlugin.getInstance().getMiniMessage().parse(format,
				Template.of("amount", "" + amount),
				Template.of("currency", currencyFormatter.apply(amount, object)));
	}
}
