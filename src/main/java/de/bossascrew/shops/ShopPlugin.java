package de.bossascrew.shops;

import co.aikar.commands.BukkitCommandManager;
import de.bossascrew.shops.commands.ShopCommand;
import de.bossascrew.shops.data.Config;
import de.bossascrew.shops.data.Database;
import de.bossascrew.shops.data.TestDatabase;
import de.bossascrew.shops.handler.*;
import de.bossascrew.shops.listener.PlayerListener;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.SneakyThrows;
import net.kyori.adventure.platform.bukkit.BukkitAudiences;
import net.kyori.adventure.text.minimessage.MiniMessage;
import net.milkbowl.vault.economy.Economy;
import org.bukkit.Bukkit;
import org.bukkit.plugin.RegisteredServiceProvider;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.Nullable;

import java.util.Locale;
import java.util.concurrent.Callable;

public class ShopPlugin extends JavaPlugin {

	//TODO datenstruktur für shop, limits..
	//TODO logs für debug policy
	//TODO database implementierung
	//TODO einzelnes leerzeichen in lore wird angezeigt
	//TODO limits und discounts in menu auflisten und mit submenü
	//TODO shop editmode basisklasse und unterer menüzeile um entries zu bearbeiten
	//TODO shop darstellung für customers

	@Getter
	private static ShopPlugin instance;

	private BukkitAudiences bukkitAudiences;
	@Getter
	private MiniMessage miniMessage;
	@Getter
	private static Economy economy = null;

	@Getter
	private Config shopsConfig;
	@Getter
	private TranslationHandler translationHandler;
	@Getter
	private Database database;
	@Getter
	private ShopHandler shopHandler;
	@Getter
	private DiscountHandler discountHandler;
	@Getter
	private LimitsHandler limitsHandler;
	@Getter
	private CustomerHandler customerHandler;

	@Getter
	private boolean loading = true;

	public ShopPlugin() {
		instance = this;
	}

	@Override
	public void onEnable() {

		//Initialize Kyori Adventure
		this.bukkitAudiences = BukkitAudiences.create(this);
		this.miniMessage = MiniMessage.get();

		//Initialize and load Config
		this.shopsConfig = new Config("config.yml");

		//Initialize Vault
		if (!setupEconomy()) {
			log(LoggingPolicy.ERROR, "Disabled due to no Vault/Economy dependency found!");
			getServer().getPluginManager().disablePlugin(this);
			return;
		}

		//Load translations
		this.translationHandler = new TranslationHandler("en_US");

		//Setup Database
		this.database = new TestDatabase(); //TODO

		//Setup ShopHandler and load shops and entrie
		this.shopHandler = new ShopHandler();
		//TODO load Shops with entries from Database
		this.shopHandler.registerDefaultShopModes();

		//Setup and load Discounts
		this.discountHandler = new DiscountHandler();

		//Setup and load Limits
		this.limitsHandler = new LimitsHandler();

		//Setup customers
		this.customerHandler = new CustomerHandler();

		//Setup inventory handler to process menus
		new InventoryHandler(this);

		//All Listeners
		Bukkit.getPluginManager().registerEvents(new PlayerListener(this), this);

		//All Commands
		BukkitCommandManager commandManager = new BukkitCommandManager(this);
		commandManager.addSupportedLanguage(Locale.ENGLISH);

		commandManager.registerCommand(new ShopCommand());

		//allow Transactions
		this.loading = false;
	}

	@Override
	public void onDisable() {

		if (this.bukkitAudiences != null) {
			this.bukkitAudiences.close();
			this.bukkitAudiences = null;
		}
	}

	public void runAsync(Runnable runnable) {
		Bukkit.getScheduler().runTaskAsynchronously(this, runnable);
	}

	public void runSync(Runnable runnable) {
		Bukkit.getScheduler().runTask(this, runnable);
	}

	@Nullable
	@SneakyThrows
	public <T> T callTaskSync(Callable<T> task) {
		if (Bukkit.isPrimaryThread()) {
			return task.call();
		}
		return Bukkit.getScheduler().callSyncMethod(this, task).get();
	}

	public BukkitAudiences getBukkitAudiences() {
		if (this.bukkitAudiences == null) {
			throw new IllegalStateException("Tried to access Adventure when the plugin was disabled!");
		}
		return this.bukkitAudiences;
	}

	private boolean setupEconomy() {
		if (getServer().getPluginManager().getPlugin("Vault") == null) {
			return false;
		}
		RegisteredServiceProvider<Economy> rsp = getServer().getServicesManager().getRegistration(Economy.class);
		if (rsp == null) {
			return false;
		}
		economy = rsp.getProvider();
		return economy != null;
	}

	/**
	 * Allows you to register your own MiniMessage, in case you want to parse the messages from the language files in a different way
	 * or have custom placeholders you want to uses.
	 *
	 * @param miniMessage your preferred MiniMessage instance
	 */
	public void registerMiniMessage(MiniMessage miniMessage) {
		this.miniMessage = miniMessage;
	}

	public void log(LoggingPolicy policy, String message) {
		if (shopsConfig.getLoggingPolicy().getPriotiry() <= policy.getPriotiry()) {
			getLogger().log(policy.getLevel(), message);
		}
	}

	public void log(LoggingPolicy policy, String message, Throwable exception) {
		if (shopsConfig.getLoggingPolicy().getPriotiry() <= policy.getPriotiry()) {
			getLogger().log(policy.getLevel(), message, exception);
		}
	}
}
