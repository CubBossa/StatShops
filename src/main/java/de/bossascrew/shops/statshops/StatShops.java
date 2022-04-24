package de.bossascrew.shops.statshops;

import co.aikar.commands.BukkitCommandManager;
import co.aikar.commands.InvalidCommandArgument;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.data.Database;
import de.bossascrew.shops.statshops.api.data.LogDatabase;
import de.bossascrew.shops.statshops.commands.ShopCommand;
import de.bossascrew.shops.statshops.convertion.DataPreset;
import de.bossascrew.shops.statshops.data.*;
import de.bossascrew.shops.statshops.handler.*;
import de.bossascrew.shops.statshops.hook.CitizensHook;
import de.bossascrew.shops.statshops.hook.VaultExtension;
import de.bossascrew.shops.statshops.listener.PlayerListener;
import lombok.Getter;
import lombok.SneakyThrows;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.platform.bukkit.BukkitAudiences;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.MiniMessage;
import org.bstats.bukkit.Metrics;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Player;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class StatShops extends JavaPlugin {

	public static final String PERM_ARTICLE_STATIC = "statshops.editor.article.static";
	public static final String PERM_ARTICLE_CLOSE_SHOP = "statshops.editor.article.close_shop";
	public static final String PERM_ARTICLE_OPEN_SHOP = "statshops.editor.article.open_shop";
	public static final String PERM_ARTICLE_NEXT_PAGE = "statshops.editor.article.next_page";
	public static final String PERM_ARTICLE_PREV_PAGE = "statshops.editor.article.prev_page";
	public static final String PERM_ARTICLE_EXACT_PAGE = "statshops.editor.article.exact_page";
	public static final String PERM_ARTICLE_TRADE_ITEM = "statshops.editor.article.trade_item";
	public static final String PERM_ARTICLE_TRADE_CMD = "statshops.editor.article.trade_cmd";
	public static final String PERM_ARTICLE_TRADE_CONSOLE_CMD = "statshops.editor.article.trade_console_cmd";
	public static final String PERM_COSTS_ITEM = "statshops.editor.costs.item";
	public static final String PERM_COSTS_XP = "statshops.editor.costs.xp";
	public static final String PERM_COSTS_VAULT = "statshops.editor.costs.vault";
	public static final String PERM_CMD_OPEN = "statshops.command.open";
	public static final String PERM_CMD_OPEN_FOR = "statshops.command.openfor";
	public static final String PERM_CMD_EDITOR = "statshops.command.editor";
	public static final String PERM_CMD_RELOAD = "statshops.command.reload";

	public static final String TAG_GLOBAL = "global";
	public static final String COMPLETION_SHOPS = "@shops";
	public static final String COMPLETION_DATA_TEMPLATES = "@data_templates";

	@Getter
	private static StatShops instance;

	@Getter
	private static final List<StatShopsExtension> registeredExtensions = new ArrayList<>();

	@Getter
	private BukkitCommandManager commandManager;
	private BukkitAudiences bukkitAudiences;
	@Getter
	private MiniMessage miniMessage;

	@Getter
	private Config shopsConfig;
	@Getter
	private TranslationHandler translationHandler;
	@Getter
	private Database database;
	@Getter
	private LogDatabase logDatabase;
	@Getter
	private SubModulesHandler subModulesHandler;
	@Getter
	private EntryModuleHandler entryModuleHandler;
	@Getter
	private ShopHandler shopHandler;
	@Getter
	private DiscountHandler discountHandler;
	@Getter
	private LimitsHandler limitsHandler;
	@Getter
	private CustomerHandler customerHandler;
	@Getter
	private TemplateHandler templateHandler;
	@Getter
	private CurrencyHandler currencyHandler;
	@Getter
	private DynamicPricingHandler dynamicPricingHandler;

	@Getter
	private Metrics metrics;

	private static boolean loading = true;

	@Getter
	private VaultExtension vaultExtension = null;
	@Getter
	private CitizensHook citizensHook = null;

	private Audience consoleAudience;

	public StatShops() {
		instance = this;
	}

	@Override
	public void onEnable() {

		// Initialize Vault
		if (Bukkit.getPluginManager().getPlugin("Vault") != null) {
			vaultExtension = new VaultExtension(this);
			if (!vaultExtension.setupEconomy()) {
				vaultExtension = null;
			}
			if (vaultExtension != null) {
				registerExtension(vaultExtension);
				log(LoggingPolicy.INFO, "Vault found and successfully hooked.");
			} else {
				log(LoggingPolicy.WARN, "Vault found but could not enable economy.");
			}
		}

		// Initialize Kyori Adventure
		this.bukkitAudiences = BukkitAudiences.create(this);
		this.miniMessage = MiniMessage.miniMessage();
		this.consoleAudience = bukkitAudiences.sender(Bukkit.getConsoleSender());

		// Initialize and load Config
		this.shopsConfig = new Config(super.getDataFolder().getPath() + "\\config.yml");
		this.shopsConfig.loadConfig();

		this.currencyHandler = new CurrencyHandler();

		// Initialize Citizens
		if (Bukkit.getPluginManager().getPlugin("Citizens") != null) {
			citizensHook = new CitizensHook(this);
			log(LoggingPolicy.INFO, "Citizens found and successfully hooked");
		}

		// Load translations
		this.translationHandler = new TranslationHandler("en_US");

		// Setup Database
		File data = new File(getDataFolder(), "data");
		data.mkdir();
		this.database = new FlatFileDatabase(data); //TODO databasehandler ...
		File logs = new File(getDataFolder(), "logs");
		logs.mkdir();
		this.logDatabase = new FlatFileLogDatabase(logs, shopsConfig.logFilePerShop, shopsConfig.logDirPerShop,
				shopsConfig.logFilePerDay, shopsConfig.logDirPerDay);

		// Register dynamic pricing
		this.dynamicPricingHandler = new DynamicPricingHandler();
		this.dynamicPricingHandler.loadDefaultPricing("statshops", dbKey -> {
			Map<String, Double> defaultValues = new HashMap<>();
			saveResource("default_values.yml", false);
			File file = new File(getDataFolder(), "default_values.yml");
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);
			for (String key : cfg.getKeys(false)) {
				Double d = cfg.getDouble(key, 10.);
				// To simplify prices for admins every key is registered without namespace as well for the default database.
				defaultValues.put(key, d);
				defaultValues.put(dbKey + ":" + key, d);
			}
			return defaultValues;
		});

		// Enable Entry Modules and Sub modules
		this.entryModuleHandler = new EntryModuleHandler();
		this.entryModuleHandler.registerDefaults();

		this.subModulesHandler = new SubModulesHandler();
		this.subModulesHandler.registerDefaults();

		// Setup ShopHandler and load shops and entries
		this.shopHandler = new ShopHandler();

		// Setup and load shop templates
		this.templateHandler = new TemplateHandler();
		this.templateHandler.registerDefaults();
		this.templateHandler.loadDefaultTemplateFromConfig(shopsConfig);

		// Load after templates to load default templates properly
		this.shopHandler.loadShopsFromDatabase(database);

		// Setup and load Discounts
		this.discountHandler = new DiscountHandler();

		// Setup and load Limits
		this.limitsHandler = new LimitsHandler();

		// Setup customers
		this.customerHandler = new CustomerHandler();

		// Setup inventory handler to process menus
		new InventoryHandler(this);

		// All Listeners
		Bukkit.getPluginManager().registerEvents(new PlayerListener(this), this);

		// All Commands

		commandManager = new BukkitCommandManager(this);
		commandManager.addSupportedLanguage(Locale.ENGLISH);
		registerContexts();

		commandManager.registerCommand(new ShopCommand());

		registerCompletions();


		// Register to metrics
		this.metrics = new Metrics(this, 13842);

		// Allow Transactions
		loading = false;
	}

	public static void registerExtension(StatShopsExtension statShopsExtension) {
		registeredExtensions.add(statShopsExtension);
	}

	public static boolean busy() {
		return loading;
	}

	public static void setBusy(boolean busy) {
		loading = busy;
	}

	public static void setBusyFor(CompletableFuture<?> future) {
		loading = true;
		future.thenAcceptAsync(o -> loading = false);
	}

	@Override
	public void onDisable() {

		InventoryHandler.getInstance().closeAllMenus(true);
		for (Shop shop : ShopHandler.getInstance().getShops()) {
			database.saveShop(shop);
			if (shopsConfig.isCleanupUnusedEntries()) {
				for (ShopEntry shopEntry : shop.getUnusedEntries()) {
					database.deleteEntry(shopEntry);
				}
			}
		}
		LimitsHandler.getInstance().getLimits().forEach(limit -> database.saveLimit(limit));
		DiscountHandler.getInstance().getDiscounts().forEach(discount -> database.saveDiscount(discount));
		TemplateHandler.getInstance().getTemplates().forEach(template -> {
			if (Arrays.stream(TemplateHandler.DEFAULT_TEMPLATES).noneMatch(uuid -> uuid.equals(template.getUuid()))) {
				database.saveTemplate(template);
			}
		});

		if (this.bukkitAudiences != null) {
			this.bukkitAudiences.close();
			this.bukkitAudiences = null;
		}
	}

	public boolean isCitizensInstalled() {
		return citizensHook != null;
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
		if (shopsConfig == null || shopsConfig.getLoggingPolicy().getPriority() <= policy.getPriority()) {
			getLogger().log(policy.getLevel(), message);
		}
	}

	public void log(LoggingPolicy policy, String message, Throwable exception) {
		if (shopsConfig == null || shopsConfig.getLoggingPolicy().getPriority() <= policy.getPriority()) {
			getLogger().log(policy.getLevel(), message, exception);
		}
	}

	public void sendMessage(CommandSender sender, Message message) {
		sendMessage(sender, message.getTranslation());
	}

	public void sendMessage(CommandSender sender, Component message) {
		sendMessage(sender, "", message, -1);
	}

	public void sendMessage(CommandSender sender, String key, Message message, int cooldown) {
		sendMessage(sender, key, message.getTranslation(), cooldown);
	}

	public void sendMessage(CommandSender sender, String key, Component message, int cooldown) {
		if (sender instanceof Player player) {
			Customer.wrap(player).sendMessage(key, message, cooldown);
		} else {
			consoleAudience.sendMessage(message);
		}
	}

	public void registerCompletions() {
		commandManager.getCommandCompletions().registerCompletion(COMPLETION_SHOPS, context -> {
			return ShopHandler.getInstance().getShopMap().values().stream()
					.map(Shop::getNamePlain)
					.map(s -> s.replaceAll(" ", "_")).
					collect(Collectors.toList());
		});
		commandManager.getCommandCompletions().registerCompletion(COMPLETION_DATA_TEMPLATES, context -> {
			return Arrays.stream(Objects.requireNonNull(new File(getDataFolder(), "presets/").listFiles()))
					.map(File::getName)
					.filter(name -> name.endsWith(DataPreset.FILE_ENDING))
					.collect(Collectors.toList());
		});
	}

	public void registerContexts() {
		commandManager.getCommandContexts().registerContext(Shop.class, context -> {
			String search = context.popFirstArg();
			Shop shop = ShopHandler.getInstance().getShopMap().values().stream()
					.filter(s -> s.getNamePlain().replaceAll(" ", "_").equalsIgnoreCase(search)).findFirst().orElse(null);
			if (shop != null) {
				return shop;
			}
			if (context.isOptional()) {
				return null;
			}
			throw new InvalidCommandArgument("Es existiert kein Shop \"" + search + "\"");
		});
	}
}
