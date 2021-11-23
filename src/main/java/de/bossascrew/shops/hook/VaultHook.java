package de.bossascrew.shops.hook;

import de.bossascrew.shops.ShopPlugin;
import net.milkbowl.vault.economy.Economy;
import org.bukkit.plugin.RegisteredServiceProvider;

public class VaultHook {

	private static Economy economy = null;

	private ShopPlugin shopPlugin;

	public VaultHook(ShopPlugin shopPlugin) {
		this.shopPlugin = shopPlugin;
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
