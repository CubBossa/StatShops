package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.ShopPlugin;
import org.bukkit.inventory.ItemStack;

import java.util.function.Function;
import java.util.function.Predicate;

public class ShopEntryType {

	private final Function<String, ItemStack> displayItemFromLanguageFunction;
	private final Predicate<ShopEntry> appliesToEntry;
	private final PayElement pay;
	private final GainElement gain;

	public ShopEntryType(Function<String, ItemStack> displayItemFromLanguageFunction, Predicate<ShopEntry> appliesToEntry,
						 PayElement payElement, GainElement gainElement) {
		this.displayItemFromLanguageFunction = displayItemFromLanguageFunction;
		this.appliesToEntry = appliesToEntry;
		this.pay = payElement;
		this.gain = gainElement;
	}

	public ItemStack getDisplayItem() {
		return displayItemFromLanguageFunction.apply(ShopPlugin.getInstance().getShopsConfig().getLanguage());
	}

	public boolean applies(ShopEntry entry) {
		return appliesToEntry.test(entry);
	}

	public EntryElement getPayElement() {
		return pay;
	}

	public EntryElement getGainElement() {
		return gain;
	}
}
