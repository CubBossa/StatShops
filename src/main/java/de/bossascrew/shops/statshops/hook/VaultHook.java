package de.bossascrew.shops.statshops.hook;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.general.handler.CurrencyHandler;
import de.bossascrew.shops.statshops.shop.Currency;
import net.kyori.adventure.text.Component;
import net.milkbowl.vault.economy.Economy;
import org.bukkit.plugin.RegisteredServiceProvider;

public class VaultHook {

	private static Economy economy = null;

	private StatShops shopPlugin;

	public static final Currency<Void> CURRENCY_VAULT = new Currency<Void>("<amount> <currency>", (integer, unused) -> {
		return Component.text(integer == 1 ? economy.currencyNameSingular() : economy.currencyNamePlural()); //TODO evtl legacy parsing
	}) {
		@Override
		public double getAmount(Customer customer, Void object) {
			return economy.getBalance(customer.getPlayer());
		}

		@Override
		public boolean addAmount(Customer customer, double amount, Void object) {
			return economy.depositPlayer(customer.getPlayer(), amount).transactionSuccess();
		}

		@Override
		public boolean removeAmount(Customer customer, double amount, Void object) {
			return economy.withdrawPlayer(customer.getPlayer(), amount).transactionSuccess();
		}
	};

	public VaultHook(StatShops shopPlugin) {
		this.shopPlugin = shopPlugin;
		CurrencyHandler.getInstance().registerCurrency("vault", CURRENCY_VAULT);
	}

	public boolean setupEconomy() {
		if (shopPlugin.getServer().getPluginManager().getPlugin("Vault") == null) {
			return false;
		}
		RegisteredServiceProvider<Economy> rsp = shopPlugin.getServer().getServicesManager().getRegistration(Economy.class);
		if (rsp == null) {
			return false;
		}
		economy = rsp.getProvider();
		return economy != null;
	}
}
