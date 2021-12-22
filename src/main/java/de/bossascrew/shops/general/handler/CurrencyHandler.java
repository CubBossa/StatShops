package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
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
			aDouble -> String.format("%.0f", aDouble), (integer, itemStack) -> TextUtils.toComponent(itemStack)) {
		@Override
		public double applyDiscount(double amount, double discount) {
			return (int) (amount * discount);
		}

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
	public static Currency<String> CURRENCY_COMMAND = new Currency<>("Command: <currency>", "Command: <currency>", (aDouble, s) -> Component.text(s)) {
		@Override
		public double applyDiscount(double amount, double discount) {
			return (int) (amount * discount);
		}

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
	public static Currency<String> CURRENCY_CONSOLE_COMMAND = new Currency<>("<amount>x Command: <currency>", "<amount>x Command: <currency>", (aDouble, s) -> Component.text(s)) {
		@Override
		public double applyDiscount(double amount, double discount) {
			return (int) (amount * discount);
		}

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
	public static final Currency<Void> CURRENCY_EXP = new Currency<Void>("<amount> Exp", "<st><amount></st> <amount_dc> Exp", (aDouble, unused) -> Component.text("Exp")) { //TODO config

		@Override
		public double applyDiscount(double amount, double discount) {
			return amount * discount; //TODO max  2,147,483,647;
		}

		@Override
		public double getAmount(Customer customer, Void object) {
			return customer.getPlayer().getExp();
		}

		@Override
		public boolean addAmount(Customer customer, double amount, Void object) {
			Player player = customer.getPlayer();
			player.setExp((float) (player.getExp() + amount));
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, Void object) {
			Player player = customer.getPlayer();
			player.setExp((float) (player.getExp() - amount));
			return true;
		}
	};

	Map<String, Currency<?>> currencies;

	public CurrencyHandler() {
		instance = this;
		this.currencies = new HashMap<>();

		registerCurrency("item", CURRENCY_ITEM);
		registerCurrency("command", CURRENCY_COMMAND);
		registerCurrency("console_command", CURRENCY_CONSOLE_COMMAND);
		registerCurrency("experiencce", CURRENCY_EXP);

		// Registering external currencies
		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.registerCurrencies(this);
		}
	}

	public <T> void registerCurrency(String key, Currency<T> currency) {
		this.currencies.put(key, currency);
	}

	public void unregisterCurrency(String key) {
		this.currencies.remove(key);
	}
}
