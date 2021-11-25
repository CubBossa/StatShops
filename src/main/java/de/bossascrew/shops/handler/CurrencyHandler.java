package de.bossascrew.shops.handler;

import de.bossascrew.shops.shop.Currency;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

import java.util.HashMap;
import java.util.Map;

public class CurrencyHandler {

	@Getter
	private static CurrencyHandler instance;

	public final Currency<ItemStack> CURRENCY_ITEM = new Currency<>("<amount>x <currency>", (integer, itemStack) -> {
		return Component.translatable("item.minecraft." + itemStack.getType().toString().toLowerCase());
	});

	Map<String, Currency<?>> currencies;

	public CurrencyHandler() {
		instance = this;
		this.currencies = new HashMap<>();

		registerCurrency("item", CURRENCY_ITEM);
	}

	public <T> void registerCurrency(String key, Currency<T> currency) {
		this.currencies.put(key, currency);
	}

	public void unregisterCurrency(String key) {
		this.currencies.remove(key);
	}
}
