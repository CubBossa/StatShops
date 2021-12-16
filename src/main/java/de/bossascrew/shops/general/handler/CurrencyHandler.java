package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.inventory.ItemStack;

import java.util.HashMap;
import java.util.Map;

public class CurrencyHandler {

	@Getter
	private static CurrencyHandler instance;

	//TODO format erst nach laden der cfg
	public static final Currency<ItemStack> CURRENCY_ITEM = new Currency<>(
			StatShops.getInstance().getShopsConfig().getCurrencyItemFormatting(),
			StatShops.getInstance().getShopsConfig().getCurrencyItemFormattingDiscounted(),
			aDouble -> String.format("%.0f", aDouble), (integer, itemStack) -> {
		return TextUtils.toComponent(itemStack);
	}) {
		@Override
		public double getAmount(Customer customer, ItemStack object) {
			int count = 0;
			for (ItemStack i : customer.getPlayer().getInventory()) {
				if (i == null) {
					continue;
				}
				if (i.isSimilar(object)) {
					count += i.getAmount();
				}
			}
			return count;
		}

		@Override
		public boolean addAmount(Customer customer, double amount, ItemStack object) {
			object = object.clone();
			object.setAmount((int) amount);
			ItemStackUtils.giveOrDrop(customer.getPlayer(), object);
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, ItemStack object) {
			amount = (int) amount;
			if (amount < 0) {
				return addAmount(customer, amount * -1, object);
			}

			Map<Integer, ItemStack> removableStacks = new HashMap<>();
			for (int slot = 0; slot < customer.getPlayer().getInventory().getSize(); slot++) {
				ItemStack i = customer.getPlayer().getInventory().getItem(slot);
				if (i == null) {
					continue;
				}
				if (i.isSimilar(object)) {
					removableStacks.put(slot, i);
				}
			}
			int removed = 0;
			for (Map.Entry<Integer, ItemStack> entry : removableStacks.entrySet()) {
				int a = entry.getValue().getAmount();
				if (removed + a > amount) {
					customer.getPlayer().getInventory().getItem(entry.getKey()).setAmount((int) (removed + a - amount));
					break;
				}
				customer.getPlayer().getInventory().setItem(entry.getKey(), null);
				removed += a;
			}
			return true;
		}
	};
	public static Currency<String> CURRENCY_COMMAND = new Currency<String>("Command: <currency>", "Command: <currency>", (aDouble, s) -> Component.text(s)) {
		@Override
		public double getAmount(Customer customer, String object) {
			return 0;
		}

		@Override
		public boolean addAmount(Customer customer, double amount, String object) {
			for (int i = 0; i < amount; i++) {
				customer.getPlayer().performCommand(object);
			}
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, String object) {
			return false;
		}
	};
	public static Currency<String> CURRENCY_CONSOLE_COMMAND = new Currency<String>("Command: <currency>", "Command: <currency>", (aDouble, s) -> Component.text(s)) {
		@Override
		public double getAmount(Customer customer, String object) {
			return 0;
		}

		@Override
		public boolean addAmount(Customer customer, double amount, String object) {
			for (int i = 0; i < amount; i++) {
				Bukkit.dispatchCommand(Bukkit.getConsoleSender(), object);
			}
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, String object) {
			return false;
		}
	};

	Map<String, Currency<?>> currencies;

	public CurrencyHandler() {
		instance = this;
		this.currencies = new HashMap<>();

		registerCurrency("item", CURRENCY_ITEM);
		registerCurrency("command", CURRENCY_COMMAND);
	}

	public <T> void registerCurrency(String key, Currency<T> currency) {
		this.currencies.put(key, currency);
	}

	public void unregisterCurrency(String key) {
		this.currencies.remove(key);
	}
}
