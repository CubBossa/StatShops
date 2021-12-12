package de.bossascrew.shops.statshops;

import co.aikar.commands.BukkitCommandManager;
import co.aikar.commands.ConditionFailedException;
import co.aikar.commands.InvalidCommandArgument;
import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.handler.CurrencyHandler;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.handler.InventoryHandler;
import de.bossascrew.shops.general.handler.TemplateHandler;
import de.bossascrew.shops.itemeditor.ItemEditorCommand;
import de.bossascrew.shops.statshops.commands.ShopCommand;
import de.bossascrew.shops.statshops.data.*;
import de.bossascrew.shops.statshops.handler.*;
import de.bossascrew.shops.statshops.hook.CitizensHook;
import de.bossascrew.shops.statshops.hook.VaultHook;
import de.bossascrew.shops.statshops.listener.PlayerListener;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.util.ItemFlags;
import de.bossascrew.shops.general.util.LoggingPolicy;
import lombok.Getter;
import lombok.SneakyThrows;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.platform.bukkit.BukkitAudiences;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.MiniMessage;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.command.CommandSender;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemFlag;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.SpawnEggMeta;
import org.bukkit.material.Colorable;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class StatShops extends JavaPlugin {

	public static final String TAG_GLOBAL = "global";
	public static final String CONDITION_ITEM_IN_HAND = "item_in_hand";
	public static final String CONDITION_ITEM_HAS_META = "item_has_meta";
	public static final String CONDITION_ITEM_SPAWNABLE = "item_spawnable";
	public static final String CONDITION_ITEM_COLORABLE = "item_colorable";
	public static final String COMPLETION_SHOPS = "@shops";
	public static final String COMPLETION_ENCHANTMENTS = "@enchantments";
	public static final String COMPLETION_ENCHANTMENTS_CONTAINED = "@enchantments_on_item";
	public static final String COMPLETION_ITEM_FLAGS = "@commandflags";

	@Getter
	private static StatShops instance;

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

	private static boolean loading = true;

	@Getter
	private VaultHook vaultHook = null;
	@Getter
	private CitizensHook citizensHook = null;

	private Audience consoleAudience;

	public StatShops() {
		instance = this;
	}

	@Override
	public void onEnable() {

		// Initialize Kyori Adventure
		this.bukkitAudiences = BukkitAudiences.create(this);
		this.miniMessage = MiniMessage.get();
		this.consoleAudience = bukkitAudiences.sender(Bukkit.getConsoleSender());

		// Initialize and load Config
		this.shopsConfig = new Config(super.getDataFolder().getPath() + "\\config.yml");
		this.shopsConfig.loadConfig();

		this.currencyHandler = new CurrencyHandler();

		// Initialize Vault
		if (Bukkit.getPluginManager().getPlugin("Vault") != null) {
			vaultHook = new VaultHook(this);
			if (vaultHook.setupEconomy()) {
				vaultHook = null;
			}
			if (isVaultInstalled()) {
				log(LoggingPolicy.INFO, "Vault found and successfully hooked.");
			} else {
				log(LoggingPolicy.WARN, "Vault found but could not enable economy.");
			}
		}

		// Initialize Citizens
		if (Bukkit.getPluginManager().getPlugin("Citizens") != null) {
			citizensHook = new CitizensHook(this);
			log(LoggingPolicy.INFO, "Citizens found and successfully hooked");
		}

		// Load translations
		this.translationHandler = new TranslationHandler("en_US");

		// Setup Database
		this.database = new TestDatabase(); //TODO
		this.logDatabase = (LogDatabase) this.database;

		// Enable Entry Modules
		this.entryModuleHandler = new EntryModuleHandler();
		this.entryModuleHandler.registerDefaults();

		// Setup ShopHandler and load shops and entries
		this.shopHandler = new ShopHandler();
		this.shopHandler.registerDefaultShopModes();

		// Setup and load shop templates
		this.templateHandler = new TemplateHandler();
		this.templateHandler.registerDefaults();

		// Load after templates to load default templates properly
		this.shopHandler.loadShopsFromDatabase(this.database);

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
		registerConditions();

		commandManager.registerCommand(new ShopCommand());
		commandManager.registerCommand(new ItemEditorCommand());

		registerCompletions();

		// Allow Transactions
		loading = false;
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

		if (this.bukkitAudiences != null) {
			this.bukkitAudiences.close();
			this.bukkitAudiences = null;
		}
	}

	public boolean isVaultInstalled() {
		return vaultHook != null;
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
		if (shopsConfig == null || shopsConfig.getLoggingPolicy().getPriotiry() <= policy.getPriotiry()) {
			getLogger().log(policy.getLevel(), message);
		}
	}

	public void log(LoggingPolicy policy, String message, Throwable exception) {
		if (shopsConfig == null || shopsConfig.getLoggingPolicy().getPriotiry() <= policy.getPriotiry()) {
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
		commandManager.getCommandCompletions().registerCompletion(COMPLETION_ENCHANTMENTS, context -> {
			return Arrays.stream(Enchantment.values()).map(enchantment -> enchantment.getKey().getKey()).collect(Collectors.toList());
		});
		commandManager.getCommandCompletions().registerCompletion(COMPLETION_ENCHANTMENTS_CONTAINED, context -> {
			if (context.getSender() instanceof Player player) {
				ItemStack hand = player.getInventory().getItemInMainHand();
				if (hand.hasItemMeta()) {
					return hand.getItemMeta().getEnchants().keySet().stream()
							.map(enchantment -> enchantment.getKey().getKey().toLowerCase())
							.collect(Collectors.toList());
				}
			}
			return null;

		});
		commandManager.getCommandCompletions().registerCompletion(COMPLETION_ITEM_FLAGS, context -> {
			String beforeLastComma = context.getInput().substring(0, context.getInput().lastIndexOf(',') + 1);
			List<String> completions = Arrays.stream(ItemFlag.values())
					.map(itemFlag -> itemFlag.toString().toLowerCase())
					.filter(itemFlag -> !beforeLastComma.toLowerCase().contains(itemFlag))
					.map(itemFlag -> beforeLastComma + itemFlag)
					.collect(Collectors.toList());
			completions.add("*");
			return completions;
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
		commandManager.getCommandContexts().registerContext(ItemFlags.class, context -> {
			if (context.getFirstArg().equalsIgnoreCase("*")) {
				return new ItemFlags(ItemFlag.values());
			}
			String[] splits = context.getFirstArg().split(",");
			ItemFlags flags = new ItemFlags();
			for (String split : splits) {
				try {
					flags.add(ItemFlag.valueOf(split.toUpperCase()));
				} catch (IllegalArgumentException e) {
					throw new InvalidCommandArgument("\"" + split + "\" is not a valid command flag.");
				}
			}
			return flags;
		});
		commandManager.getCommandContexts().registerContext(Enchantment.class, context -> {
			String input = context.popFirstArg();
			Enchantment e = Enchantment.getByKey(NamespacedKey.minecraft(input));
			if (e == null) {
				throw new InvalidCommandArgument("There is no enchantment with the name \"" + input + "\".");
			}
			return e;
		});
	}

	public void registerConditions() {
		commandManager.getCommandConditions().addCondition(CONDITION_ITEM_IN_HAND, context -> {
			if (!(context.getIssuer().getIssuer() instanceof Player player) || player.getInventory().getItemInMainHand().getType() == Material.AIR) {
				throw new ConditionFailedException("You need to hold an Item in your main hand to run this command."); //TODO translations
			}
		});
		commandManager.getCommandConditions().addCondition(CONDITION_ITEM_HAS_META, context -> {
			if (context.getIssuer().getIssuer() instanceof Player player) {
				ItemStack hand = player.getInventory().getItemInMainHand();
				if (!hand.hasItemMeta()) {
					hand.setItemMeta(Bukkit.getItemFactory().getItemMeta(hand.getType()));
				}
			}
		});
		commandManager.getCommandConditions().addCondition(CONDITION_ITEM_SPAWNABLE, context -> {
			ItemStack stack = context.getIssuer().getPlayer().getInventory().getItemInMainHand();
			if (!(stack.getItemMeta() instanceof SpawnEggMeta)) {
				throw new ConditionFailedException("The item in your main hand needs to be a spawner or a spawn egg.");
			}
		});
		commandManager.getCommandConditions().addCondition(CONDITION_ITEM_COLORABLE, context -> {
			ItemStack stack = context.getIssuer().getPlayer().getInventory().getItemInMainHand();
			if (!(stack.getItemMeta() instanceof Colorable)) {
				throw new ConditionFailedException("The item in your main hand needs to be a colorable object.");
			}
		});
	}
}
