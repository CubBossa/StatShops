package de.bossascrew.shops.statshops.convertion;

import com.google.common.collect.Lists;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.api.module.MultiTradeModule;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.hook.VaultExtension;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.entry.ArticleSubModule;
import de.bossascrew.shops.statshops.shop.entry.BaseEntry;
import de.bossascrew.shops.statshops.shop.entry.CostsSubModule;
import de.bossascrew.shops.statshops.shop.entry.MultiTradeBaseModule;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

public class DtlTradersConverter implements Converter {

	public static final String ARG_PAGINATION = "bottom_line";
	public static final String ARG_LOGS = "logs";

	private final File config;
	private final File language;
	private final File shopsDirectory;
	private @Nullable EntryTemplate template = null;

	private String shopTitleFormat;
	private String modeBuyName;
	private String modeSellName;
	private String modeTradeName;

	private int modeSlot;
	private String modePosition;
	private ItemStack modeBuy;
	private ItemStack modeSell;
	private ItemStack modeTrade;

	public DtlTradersConverter(File config, File language, File shopsDirectory) {
		this.config = config;
		this.language = language;
		this.shopsDirectory = shopsDirectory;
	}


	@Override
	public void convert(String... args) {
		List<String> argsList = Lists.newArrayList(args);

		YamlConfiguration configuration = YamlConfiguration.loadConfiguration(config);

		//title and names
		loadShopTitleFormat(YamlConfiguration.loadConfiguration(language));

		//next ,back, exit
		if (argsList.contains(ARG_PAGINATION)) {
			loadEntryTemplate(configuration);
		}
		//buy, sell, trade
		loadShopModeIcons(configuration);

		//shops:

	}

	private void loadShopTitleFormat(YamlConfiguration configuration) {
		shopTitleFormat = configuration.getString("Shop.shop-title");
		modeBuyName = configuration.getString("Shop.shop-mode-buy");
		modeSellName = configuration.getString("Shop.shop-mode-sell");
		modeTradeName = configuration.getString("Shop.shop-mode-trade");
	}

	private void loadShops() {
		for (File file : Objects.requireNonNull(shopsDirectory.listFiles())) {
			if (!file.getName().endsWith(".yml")) {
				continue;
			}
			YamlConfiguration configuration = YamlConfiguration.loadConfiguration(file);
			ConfigurationSection cfg = configuration.getConfigurationSection(new ArrayList<>(configuration.getKeys(false)).get(0));

			boolean modeBuy = cfg.getBoolean("buy-shop-enabled");
			boolean modeSell = cfg.getBoolean("sell-shop-enabled");
			boolean modeTrade = cfg.getBoolean("trade-shop-enabled");

			String title = cfg.getString("title");
			String partTitle = shopTitleFormat.replace("%shop-name%", title);
			partTitle.replace("%page-name%", "");

			Shop shopBuy = !modeBuy ? null : ShopHandler.getInstance().createShop(partTitle.replace("%shop-mode%", modeBuyName), ChestMenuShop.class);
			Shop shopSell = !modeBuy ? null : ShopHandler.getInstance().createShop(partTitle.replace("%shop-mode%", modeSellName), ChestMenuShop.class);
			Shop shopTrade = !modeBuy ? null : ShopHandler.getInstance().createShop(partTitle.replace("%shop-mode%", modeTradeName), ChestMenuShop.class);
			List<Shop> shops = new ArrayList<>();
			if (modeBuy) {
				shops.add(shopBuy);
			}
			if (modeSell) {
				shops.add(shopSell);
			}
			if (modeTrade) {
				shops.add(shopTrade);
			}
			// lost: command
			shops.forEach(shop -> shop.setPermission(cfg.getString("permission")));
			// lost: close after purchase
			// lost: confirmation window enabled

			//TODO buy-items section

			int largestPageInRows = 1;
			// for each page
			List<Page> pages = new ArrayList<>(); //TODO die page information in den shop einarbeiten
			for (ConfigurationSection page : cfg.getConfigurationSection("pages").getKeys(false).stream().map(cfg::getConfigurationSection).collect(Collectors.toList())) {
				int size = page.getInt("size");
				if (size / 9 > largestPageInRows) {
					largestPageInRows = size / 9;
				}
				int index = Integer.parseInt(page.getName().replace("page-", ""));
				String pageName = page.getString("page-name");
				String pagePermission = page.getString("page-permission");
				Page pageObject = new Page(index, pagePermission, pageName);
				pages.add(pageObject);

				// for each buy item
				ConfigurationSection buySection = page.getConfigurationSection("buy-items");
				if (shopBuy != null && buySection != null) {
					for (ConfigurationSection item : page.getKeys(false).stream().map(page::getConfigurationSection).collect(Collectors.toList())) {
						BaseEntry entry = parseBuyEntry(shopBuy, item);
						shopBuy.addEntry(entry, entry.getSlot());
					}
				}
				ConfigurationSection sellSection = page.getConfigurationSection("sell-items");
				if (shopSell != null && sellSection != null) {
					for (ConfigurationSection item : page.getKeys(false).stream().map(page::getConfigurationSection).collect(Collectors.toList())) {
						BaseEntry entry = parseBuyEntry(shopBuy, item); //TODO parseSell
						shopSell.addEntry(entry, entry.getSlot());
					}
				}
				ConfigurationSection tradeSection = page.getConfigurationSection("trade-items");
				if (shopTrade != null && tradeSection != null) {
					for (ConfigurationSection item : page.getKeys(false).stream().map(page::getConfigurationSection).collect(Collectors.toList())) {
						BaseEntry entry = parseBuyEntry(shopBuy, item); //TODO parse trade
						shopTrade.addEntry(entry, entry.getSlot());
					}
				}
			}

			//buy sell and trade icon if has next shop
			placeShopModeIcons(shopBuy, shopSell, shopTrade);
		}
	}

	/**
	 * Parses all buy items. All pay currencies are vault
	 */
	private BaseEntry parseBuyEntry(Shop shop, ConfigurationSection entrySection) {

		ItemStack displayItem = entrySection.getItemStack("item");
		String type = entrySection.getString("type");

		String permission = entrySection.getString("permission", null);
		int slot = Integer.parseInt(entrySection.getName().replace("item-", ""));

		BaseEntry entry = new BaseEntry(UUID.randomUUID(), shop, displayItem, permission == null || permission.isEmpty() ? null : permission, slot);
		List<String> displayLore = entrySection.getBoolean("show-description") ? entrySection.getStringList("description") : new ArrayList<>();
		entry.setInfoLoreFormat(StatShops.getInstance().getMiniMessage().serialize(TextUtils.fromChatLegacy(String.join("\n", displayLore))));

		boolean trade = type.equalsIgnoreCase("trade");
		boolean commands = type.equalsIgnoreCase("commands");
		if (trade || commands) {
			double price = entrySection.getDouble("trade-price");
			CostsSubModule<?> costs = VaultExtension.COSTS_VAULT_PROVIDER.getModule(entry);
			costs.setCosts(price, price);

			List<ArticleSubModule<?>> articleSubModules = new ArrayList<>();
			if (trade) {
				ArticleSubModule.ItemArticle itemArticle = (ArticleSubModule.ItemArticle) SubModulesHandler.ARTICLE_ITEM_PROVIDER.getModule(entry);
				ItemStack gain = entrySection.getItemStack("item");
				gain.setAmount(1);
				itemArticle.setGainPrice(gain, gain.getAmount());
				articleSubModules.add(itemArticle);
			}
			if (commands) {
				ConfigurationSection commandsSection = entrySection.getConfigurationSection("commands");
				if (commandsSection != null) {
					for (ConfigurationSection commandSection : commandsSection.getKeys(false).stream()
							.map(commandsSection::getConfigurationSection).collect(Collectors.toList())) {
						String commandName = commandSection.getString("command");
						ArticleSubModule.BaseCommandArticle commandArticle;
						if (commandSection.getString("executor").equals("CONSOLE")) {
							commandArticle = (ArticleSubModule.CommandArticle) SubModulesHandler.ARTICLE_CMD_PROVIDER.getModule(entry);
						} else {
							commandArticle = (ArticleSubModule.BaseCommandArticle) SubModulesHandler.ARTICLE_CONSOLE_CMD_PROVIDER.getModule(entry);
						}
						commandArticle.setCommand(commandName);
					}
				}
				ArticleSubModule<String> commandArticle = SubModulesHandler.ARTICLE_CMD_PROVIDER.getModule(entry);
				articleSubModules.add(commandArticle);
			}
			MultiTradeModule tradeModule = new MultiTradeBaseModule(entry, null, articleSubModules, Lists.newArrayList(costs));
			tradeModule.setPurchasableStacked(true);
			//only buy items are processed in this method
			tradeModule.setSellable(false);
		}
		return entry;
	}

	private void loadEntryTemplate(YamlConfiguration cfg) {
		EntryTemplate template = new EntryTemplate(UUID.randomUUID(), "<white>DtlTraders Import</white>");
		ConfigurationSection pageSelectors = cfg.getConfigurationSection("page-selectors");

		loadEntry(template, pageSelectors.getConfigurationSection("back"), shopEntry -> EntryModuleHandler.openPrevPage(shopEntry, 1));
		loadEntry(template, pageSelectors.getConfigurationSection("next"), shopEntry -> EntryModuleHandler.openNextPage(shopEntry, 1));
		loadEntry(template, pageSelectors.getConfigurationSection("exit"), EntryModuleHandler::closeShop);

		this.template = template;
	}

	private void loadEntry(EntryTemplate template, ConfigurationSection section, Function<ShopEntry, EntryModule> moduleFunction) {
		ItemStack stack = section.getItemStack("item");
		List<Integer> slots = section.getIntegerList("slots");
		String position = section.getString("position");
		String name = section.getString("name");

		ItemStackUtils.setDisplayName(stack, TextUtils.fromChatLegacy(name));

		for (int slot : slots) {
			boolean bottom = position.equalsIgnoreCase("bottom");
			ShopEntry shopEntry = new BaseEntry(UUID.randomUUID(), null, stack, null, slot);
			shopEntry.setModule(moduleFunction.apply(shopEntry));
			template.put(bottom ? "(<row> - 1) * 9 + " + slot : "" + slot, shopEntry);
		}
	}

	private void loadShopModeIcons(YamlConfiguration cfg) {
		ConfigurationSection shopSelectors = cfg.getConfigurationSection("shop-selectors");
		modeSlot = shopSelectors.getInt("slot");
		modePosition = shopSelectors.getString("position");

		modeBuy = shopSelectors.getItemStack("buy.item");
		modeBuy = ItemStackUtils.setDisplayName(modeBuy, TextUtils.fromChatLegacy(shopSelectors.getString("buy.name")));
		modeBuy = shopSelectors.getItemStack("sell.item");
		modeBuy = ItemStackUtils.setDisplayName(modeBuy, TextUtils.fromChatLegacy(shopSelectors.getString("sell.name")));
		modeBuy = shopSelectors.getItemStack("trade.item");
		modeBuy = ItemStackUtils.setDisplayName(modeBuy, TextUtils.fromChatLegacy(shopSelectors.getString("trade.name")));
	}

	private void placeShopModeIcons(Shop shopBuy, Shop shopSell, Shop shopTrade) {
		boolean isBuy = shopBuy != null;
		boolean isSell = shopSell != null;
		boolean isTrade = shopTrade != null;

		if (isBuy && (isSell || isTrade)) {
			ShopEntry entry = shopBuy.createEntry(modeBuy, modeSlot);
			entry.setModule(EntryModuleHandler.openShop(entry, isSell ? shopSell : shopTrade));
		}
		if (isSell && (isTrade || isBuy)) {
			ShopEntry entry = shopSell.createEntry(modeBuy, modeSlot);
			entry.setModule(EntryModuleHandler.openShop(entry, isTrade ? shopTrade : shopBuy));
		}
		if (isTrade && (isBuy || isSell)) {
			ShopEntry entry = shopTrade.createEntry(modeBuy, modeSlot);
			entry.setModule(EntryModuleHandler.openShop(entry, isBuy ? shopBuy : shopSell));
		}
	}

	public record Page(int page, String permission, String title) {

	}
}
