package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.general.util.ExperienceManager;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.MiniMessage;
import org.bukkit.Bukkit;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

public class CurrencyHandler {

	@Getter
	private static CurrencyHandler instance;

	//TODO format erst nach laden der cfg
	public static final Currency<ItemStack> CURRENCY_ITEM = new Currency<>(
			"item", StatShops.getInstance().getShopsConfig().getCurrencyItemFormatting(),
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
	public static Currency<String> CURRENCY_COMMAND = new Currency<>(
			"command",
			"<amount>x Command: Command: <currency>",
			"<st><amount></st> <amount>x <currency>",
			aDouble -> String.format("%.0f", aDouble),
			(aDouble, s) -> Component.text(s)) {
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
				customer.getPlayer().performCommand(object.replace("<player>", customer.getPlayer().getName()));
			}
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, String object) {
			return false;
		}
	};
	public static Currency<String> CURRENCY_CONSOLE_COMMAND = new Currency<>(
			"console_command",
			"<amount>x Command: <currency>",
			"<st><amount></st> <amount>x Command: <currency>",
			aDouble -> String.format("%.0f", aDouble),
			(aDouble, s) -> Component.text(s)) {
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
				Bukkit.dispatchCommand(Bukkit.getConsoleSender(), object.replace("<player>", customer.getPlayer().getName()));
			}
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, String object) {
			return false;
		}
	};
	public static final Currency<Void> CURRENCY_EXP = new Currency<>(
			"experience", "<amount> <#B9E45A>E<#39904C>x<#D8E45A>p",
			"<st><amount></st> <amount_dc> <#B9E45A>E<#39904C>x<#D8E45A>p",
			d -> d.intValue() + "",
			(d, unused) -> MiniMessage.miniMessage().deserialize("<#B9E45A>E<#39904C>x<#D8E45A>p")) { //TODO config

		@Override
		public double applyDiscount(double amount, double discount) {
			return amount * discount; //TODO max  2,147,483,647;
		}

		@Override
		public double getAmount(Customer customer, Void object) {
			ExperienceManager experienceManager = new ExperienceManager(customer.getPlayer());
			return experienceManager.getTotalExperience();
		}

		@Override
		public boolean addAmount(Customer customer, double amount, Void object) {
			ExperienceManager experienceManager = new ExperienceManager(customer.getPlayer());
			experienceManager.setTotalExperience(experienceManager.getTotalExperience() + (int) (amount));
			return true;
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, Void object) {
			ExperienceManager experienceManager = new ExperienceManager(customer.getPlayer());
			experienceManager.setTotalExperience(experienceManager.getTotalExperience() - (int) (amount));
			return true;
		}
	};

	Map<String, Currency<?>> currencies;

	public CurrencyHandler() {
		instance = this;
		this.currencies = new HashMap<>();

		registerCurrency(CURRENCY_ITEM);
		registerCurrency(CURRENCY_COMMAND);
		registerCurrency(CURRENCY_CONSOLE_COMMAND);
		registerCurrency(CURRENCY_EXP);

		// Registering external currencies
		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.registerCurrencies(this);
		}
	}

	public @Nullable Currency<?> getCurrency(String key) {
		return currencies.get(key);
	}

	public <T> void registerCurrency(Currency<T> currency) {
		this.currencies.put(currency.getKey(), currency);
	}

	public void unregisterCurrency(String key) {
		this.currencies.remove(key);
	}
}
