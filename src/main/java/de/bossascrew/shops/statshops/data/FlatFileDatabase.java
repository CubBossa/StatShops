package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.TemplateHandler;
import de.bossascrew.shops.general.util.*;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.hook.VaultExtension;
import de.bossascrew.shops.statshops.shop.*;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.entry.*;
import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.configuration.serialization.ConfigurationSerialization;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.IOException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class FlatFileDatabase implements Database {

	public static final String SHOP_TYPE_CHEST = "chest_menu_shop";
	public static final String SHOP_TYPE_VILLAGER = "villager_shop";

	private final File dirShops;
	private final File dirLimits;
	private final File dirDiscounts;
	private final File dirTemplate;

	public FlatFileDatabase(File directory) {
		if (!directory.isDirectory()) {
			throw new IllegalArgumentException("Input file must be a directory");
		}
		dirShops = new File(directory, "shops/");
		if (!dirShops.exists()) {
			dirShops.mkdir();
		}
		dirLimits = new File(directory, "limits/");
		if (!dirLimits.exists()) {
			dirLimits.mkdir();
		}
		dirDiscounts = new File(directory, "discounts/");
		if (!dirDiscounts.exists()) {
			dirDiscounts.mkdir();
		}
		dirTemplate = new File(directory, "template/");
		if (!dirTemplate.exists()) {
			dirTemplate.mkdir();
		}

		ConfigurationSerialization.registerClass(DataSlot.EquationSlot.class);
		ConfigurationSerialization.registerClass(DataSlot.ItemStackSlot.class);
		ConfigurationSerialization.registerClass(DataSlot.BooleanSlot.class);
		ConfigurationSerialization.registerClass(DataSlot.NumberSlot.class);
		ConfigurationSerialization.registerClass(DataSlot.ShopSlot.class);
		ConfigurationSerialization.registerClass(Price.class);
		ConfigurationSerialization.registerClass(CloseModule.class);
		ConfigurationSerialization.registerClass(OpenShopModule.class);
		ConfigurationSerialization.registerClass(PageBaseModule.class);
		ConfigurationSerialization.registerClass(TradeBaseModule.class);
		ConfigurationSerialization.registerClass(CostsSubModule.ItemCosts.class);
		ConfigurationSerialization.registerClass(CostsSubModule.ExpCosts.class);
		ConfigurationSerialization.registerClass(VaultExtension.MoneyCosts.class);
		ConfigurationSerialization.registerClass(ArticleSubModule.ItemArticle.class);
		ConfigurationSerialization.registerClass(ArticleSubModule.CommandArticle.class);
		ConfigurationSerialization.registerClass(ArticleSubModule.ConsoleCommandArticle.class);
	}

	@Override
	public Customer loadCustomer(UUID uuid) {
		return new Customer(Bukkit.getPlayer(uuid), new HashMap<>());
	}

	@Override
	public void saveCustomer(Customer customer) {

	}

	@Override
	public Map<UUID, Shop> loadShops() {
		Map<UUID, Shop> result = new HashMap<>();
		for (File file : Arrays.stream(dirShops.listFiles()).filter(file -> file.getName().endsWith(".yml")).collect(Collectors.toList())) {
			UUID uuid;
			try {
				uuid = UUID.fromString(file.getName().replace(".yml", ""));
			} catch (IllegalArgumentException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not load shop from file " + file.getName());
				continue;
			}
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);
			String type = cfg.getString("type");
			if (type == null) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not load shop, no type provided.");
				continue;
			}

			String nameFormat = cfg.getString("name-format", "error");
			String permission = cfg.getString("permission", "");
			permission = permission.isEmpty() ? null : permission;

			String defaultTemplateUuidString = cfg.getString("default-template");
			ItemStack displayItem = YamlUtils.loadSkull(cfg, "display-item");

			List<String> tags = cfg.getStringList("tags");
			Map<Integer, String> pageTitles = new HashMap<>();
			ConfigurationSection pageTitlesSection = cfg.getConfigurationSection("page-titles");
			if (pageTitlesSection != null) {
				for (String key : pageTitlesSection.getKeys(false)) {
					pageTitles.put(Integer.parseInt(key), pageTitlesSection.getString(key));
				}
			}
			Shop shop;
			switch (type) {
				case SHOP_TYPE_CHEST -> {
					int defaultPage = cfg.getInt("default-page", 0);
					boolean pageRemembered = cfg.getBoolean("page-remembered", false);
					int rows = cfg.getInt("rows");

					shop = new ChestMenuShop(nameFormat, uuid);
					ChestMenuShop cShop = (ChestMenuShop) shop;
					cShop.setDefaultPage(defaultPage);
					cShop.setPageRemembered(pageRemembered);
					cShop.setRows(rows);
					pageTitles.forEach(cShop::setPageTitle);
				}
				case SHOP_TYPE_VILLAGER -> {

					shop = new VillagerShop(nameFormat, uuid);
				}
				default -> {
					StatShops.getInstance().log(LoggingPolicy.ERROR, "Unknown shop type: " + type);
					continue;
				}
			}
			shop.setPermission(permission);
			tags.forEach(shop::addTag);
			shop.setDisplayItem(displayItem);

			//load before template so template does not get applied to empty shop page when adding first entry
			loadEntries(shop).forEach((uuid1, entry) -> shop.addEntry(entry, entry.getSlot()));

			if (defaultTemplateUuidString != null && !defaultTemplateUuidString.isEmpty()) {
				UUID defaultTemplateUuid = UUID.fromString(defaultTemplateUuidString);
				EntryTemplate template = TemplateHandler.getInstance().getTemplate(defaultTemplateUuid);
				shop.setDefaultTemplate(template);
			}

			result.put(uuid, shop);
		}
		if (result.size() > 0) {
			StatShops.getInstance().log(LoggingPolicy.INFO, "Successfully loaded " + result.size() + " shops.");
		}
		return result;
	}

	@Override
	public void saveShop(Shop shop) {
		File shopFile = new File(dirShops, shop.getUUID() + ".yml");
		if (!shopFile.exists()) {
			try {
				shopFile.createNewFile();
			} catch (IOException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not create shop file: " + shopFile.getName(), e);
				return;
			}
		}
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(shopFile);
		String type;
		if (ChestMenuShop.class.equals(shop.getClass())) {
			type = SHOP_TYPE_CHEST;
		} else if (VillagerShop.class.equals(shop.getClass())) {
			type = SHOP_TYPE_VILLAGER;
		} else {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not save shop of unknown shop type: " + shop.getClass().getTypeName());
			return;
		}
		cfg.set("type", type);
		cfg.set("name-format", shop.getNameFormat());
		cfg.set("permission", shop.getPermission() == null ? "" : shop.getPermission());
		cfg.set("default-template", shop.getDefaultTemplate() == null ? "" : shop.getDefaultTemplate().getUuid().toString());
		YamlUtils.saveSkull(cfg, "display-item", shop.getDisplayItem());

		if (type.equals(SHOP_TYPE_CHEST)) {
			ChestMenuShop cShop = (ChestMenuShop) shop;
			cfg.set("default-page", cShop.getDefaultPage());
			cfg.set("page-remembered", cShop.isPageRemembered());
			cfg.set("rows", cShop.getRows());

			cShop.getPageTitles().forEach((integer, string) -> cfg.set("page-titles." + integer, string));
		}
		cfg.set("tags", shop.getTags(false));

		shop.getEntries().forEach((integer, entry) -> saveEntry(entry));

		try {
			cfg.save(shopFile);
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Error occurred while saving shop to file: " + shopFile.getName(), e);
		}
	}

	@Override
	public void deleteShop(Shop shop) {
		File file = new File(dirShops, shop.getUUID() + ".yml");
		if (file.exists()) {
			file.delete();
		}

	}

	@Override
	public Map<UUID, ShopEntry> loadEntries(Shop shop) {
		Map<UUID, ShopEntry> result = new TreeMap<>();

		File shopFile = new File(dirShops, shop.getUUID() + ".yml");
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(shopFile);
		ConfigurationSection entrySection = cfg.getConfigurationSection("entries");
		if (entrySection == null) {
			return result;
		}
		for (String key : entrySection.getKeys(false)) {
			ConfigurationSection s = entrySection.getConfigurationSection(key);

			ShopEntry entry = loadEntry(s, shop);
			result.put(entry.getUUID(), entry);
		}
		return result;
	}

	private ShopEntry loadEntry(ConfigurationSection section, @Nullable Shop shop) {
		UUID uuid = UUID.fromString(section.getName());
		int slot = section.getInt("slot");
		ItemStack displayItem = YamlUtils.loadSkull(section, "display-item");
		String infoLoreFormat = section.getString("info-lore");
		String permission = section.getString("permission", "");
		permission = permission.isEmpty() ? null : permission;

		List<String> tags = section.getStringList("tags");


		Map<String, DataSlot<?>> dataMap = new HashMap<>();
		ConfigurationSection dataSection = section.getConfigurationSection("data-slots");
		if (dataSection != null) {
			for (String key : dataSection.getKeys(false)) {
				DataSlot<?> dataSlot = (DataSlot<?>) dataSection.get(key, DataSlot.class);
				dataMap.put(key, dataSlot);
			}
		}
		EntryModule module = section.getObject("module", EntryModule.class);

		ShopEntry entry = new BaseEntry(uuid, shop, displayItem, permission, slot);
		tags.forEach(entry::addTag);
		entry.setInfoLoreFormat(infoLoreFormat);

		if (module != null) {
			module.setShopEntry(entry);
			entry.setModule(module);
		}

		if (!dataMap.isEmpty()) {
			entry.getData().putAll(dataMap);
		}
		return entry;
	}

	@Override
	public void saveEntry(ShopEntry shopEntry) { //TODO sometimes creates two entries on same slot?

		File shopFile = new File(dirShops, shopEntry.getShop().getUUID() + ".yml");
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(shopFile);
		ConfigurationSection entrySection = cfg.getConfigurationSection("entries");
		if (entrySection == null) {
			entrySection = cfg.createSection("entries");
		}
		saveEntry(entrySection, shopEntry);
		try {
			cfg.save(shopFile);
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "An error occurred while saving shop entry: " + shopEntry.getUUID(), e);
		}
	}

	private void saveEntry(ConfigurationSection entrySection, ShopEntry shopEntry) {
		ConfigurationSection s = entrySection.getConfigurationSection(shopEntry.getUUID() + "");
		if (s == null) {
			s = entrySection.createSection(shopEntry.getUUID() + "");
		}
		s.set("slot", shopEntry.getSlot());
		YamlUtils.saveSkull(s, "display-item", shopEntry.getDisplayItem());
		s.set("info-lore", shopEntry.getInfoLoreFormat());
		s.set("permission", shopEntry.getPermission());
		s.set("tags", shopEntry.getTags(false));

		s.set("module", shopEntry.getModule());
		ConfigurationSection dataSlots = s.getConfigurationSection("data-slots");
		if (dataSlots == null) {
			dataSlots = s.createSection("data-slots");
		}
		for (Map.Entry<String, DataSlot<?>> dataSlot : shopEntry.getData().entrySet()) {
			dataSlots.set(dataSlot.getKey(), dataSlot.getValue());
		}
	}

	@Override
	public void deleteEntry(ShopEntry shopEntry) {
		File shopFile = new File(dirShops, shopEntry.getShop().getUUID() + ".yml");
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(shopFile);
		ConfigurationSection entrySection = cfg.getConfigurationSection("entries");
		if (entrySection == null) {
			return;
		}
		deleteEntry(entrySection, shopEntry);
		try {
			cfg.save(shopFile);
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "An error occurred while deleting shop entry: " + shopEntry.getUUID(), e);
		}
	}

	private void deleteEntry(ConfigurationSection entries, ShopEntry shopEntry) {
		entries.set(shopEntry.getUUID().toString(), null);
	}

	@Override
	public Map<UUID, Discount> loadDiscounts() {
		Map<UUID, Discount> result = new HashMap<>();
		for (File file : Arrays.stream(dirDiscounts.listFiles()).filter(file -> file.getName().endsWith(".yml")).collect(Collectors.toList())) {
			UUID uuid;
			try {
				uuid = UUID.fromString(file.getName().replace(".yml", ""));
			} catch (IllegalArgumentException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not load discount from file " + file.getName());
				continue;
			}
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);

			String nameFormat = cfg.getString("name-format", "error");
			String permission = cfg.getString("permission", "");
			permission = permission.isEmpty() ? null : permission;

			Duration duration = new DurationParser(true).parse(cfg.getString("duration", ""));
			List<String> tags = cfg.getStringList("tags");
			List<LocalDateTime> startTimes = cfg.getStringList("start-times").stream()
					.map(string -> LocalDateTime.parse(string, TextUtils.DATE_TIME_FORMATTER))
					.collect(Collectors.toList());
			double percent = cfg.getDouble("percent");

			Discount discount = new Discount(uuid, nameFormat, new TreeSet<>(startTimes), duration, percent, permission);
			tags.forEach(discount::addTag);

			result.put(uuid, discount);
		}
		if (result.size() > 0) {
			StatShops.getInstance().log(LoggingPolicy.INFO, "Successfully loaded " + result.size() + " discounts.");
		}
		return result;
	}

	@Override
	public void saveDiscount(Discount discount) {
		File file = new File(dirDiscounts, discount.getUuid() + ".yml");
		if (!file.exists()) {
			try {
				file.createNewFile();
			} catch (IOException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not create discount file: " + file.getName(), e);
				return;
			}
		}
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);

		cfg.set("name-format", discount.getNameFormat());
		cfg.set("duration", new DurationParser(true).format(discount.getDuration()));
		cfg.set("permission", discount.getPermission());
		cfg.set("percent", discount.getPercent());
		cfg.set("start-times", discount.getStartTimes().stream()
				.map(TextUtils.DATE_TIME_FORMATTER::format)
				.collect(Collectors.toList()));
		cfg.set("tags", discount.getTags(false));

		try {
			cfg.save(file);
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "An error occurred while saving discount: " + discount.getUuid(), e);
		}
	}

	@Override
	public void deleteDiscount(Discount discount) {
		File file = new File(dirDiscounts, discount.getUuid() + ".yml");
		if (file.exists()) {
			file.delete();
		}
	}

	@Override
	public Map<UUID, Limit> loadLimits() {
		Map<UUID, Limit> result = new HashMap<>();
		for (File file : Arrays.stream(dirLimits.listFiles()).filter(file -> file.getName().endsWith(".yml")).collect(Collectors.toList())) {
			UUID uuid;
			try {
				uuid = UUID.fromString(file.getName().replace(".yml", ""));
			} catch (IllegalArgumentException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not load limit from file " + file.getName());
				continue;
			}
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);

			String nameFormat = cfg.getString("name-format", "error");
			String permission = cfg.getString("permission", "");
			permission = permission.isEmpty() ? null : permission;

			int transactionLimit = cfg.getInt("transaction-limit");
			Duration recover = new DurationParser(true).parse(cfg.getString("recover", ""));
			boolean global = cfg.getBoolean("global");
			List<String> tags = cfg.getStringList("tags");

			Limit limit = new Limit(uuid, nameFormat, recover, customer -> true, transactionLimit);
			tags.forEach(limit::addTag);
			limit.setPermission(permission);
			limit.setGlobal(global);

			result.put(uuid, limit);
		}
		if (result.size() > 0) {
			StatShops.getInstance().log(LoggingPolicy.INFO, "Successfully loaded " + result.size() + " limits.");
		}
		return result;
	}

	@Override
	public void saveLimit(Limit limit) {
		File file = new File(dirLimits, limit.getUuid() + ".yml");
		if (!file.exists()) {
			try {
				file.createNewFile();
			} catch (IOException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not create limit file: " + file.getName(), e);
				return;
			}
		}
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);

		cfg.set("name-format", limit.getNameFormat());
		cfg.set("permission", limit.getPermission());
		cfg.set("recover", new DurationParser(true).format(limit.getRecover()));
		cfg.set("transaction-limit", limit.getTransactionLimit());
		cfg.set("global", limit.isGlobal());
		cfg.set("tags", limit.getTags(false));

		try {
			cfg.save(file);
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "An error occurred while saving limit: " + limit.getUuid(), e);
		}
	}

	@Override
	public void deleteLimit(Limit limit) {
		File file = new File(dirLimits, limit.getUuid() + ".yml");
		if (file.exists()) {
			file.delete();
		}
	}

	@Override
	public Map<UUID, EntryTemplate> loadTemplates() {
		Map<UUID, EntryTemplate> result = new HashMap<>();
		for (File file : Arrays.stream(dirTemplate.listFiles()).filter(file -> file.getName().endsWith(".yml")).collect(Collectors.toList())) {
			UUID uuid;
			try {
				uuid = UUID.fromString(file.getName().replace(".yml", ""));
			} catch (IllegalArgumentException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not load limit from file " + file.getName());
				continue;
			}
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);

			String nameFormat = cfg.getString("name-format");
			int discIndex = cfg.getInt("disc-index");

			EntryTemplate template = new EntryTemplate(uuid, nameFormat);
			template.setDiscIndex((short) discIndex);

			List<ShopEntry> loadedEntries = new ArrayList<>();
			ConfigurationSection entrySection = cfg.getConfigurationSection("entries");
			if (entrySection != null) {
				for (String key : entrySection.getKeys(false)) {
					ConfigurationSection section = entrySection.getConfigurationSection(key);
					template.put(section.getString("expression"), loadEntry(section, null));
				}
			}
			result.put(template.getUuid(), template);
		}
		if (result.size() > 0) {
			StatShops.getInstance().log(LoggingPolicy.INFO, "Successfully loaded " + result.size() + " templates.");
		}
		return result;
	}

	@Override
	public void saveTemplate(EntryTemplate template) {
		File file = new File(dirTemplate, template.getUuid() + ".yml");
		if (!file.exists()) {
			try {
				file.createNewFile();
			} catch (IOException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not create limit file: " + file.getName(), e);
				return;
			}
		}
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);

		cfg.set("name-format", template.getNameFormat());
		cfg.set("disc-index", template.getDiscIndex());
		ConfigurationSection entries = cfg.getConfigurationSection("entries");
		if (entries == null) {
			entries = cfg.createSection("entries");
		}
		for (Pair<ShopEntry, String> pair : template.getEntries()) {
			saveEntry(entries, pair.getLeft());
			entries.set(pair.getLeft().getUUID() + ".expression", pair.getRight());
		}

		try {
			cfg.save(file);
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "An error occurred while saving template: " + template.getUuid(), e);
		}
	}

	@Override
	public void deleteTemplate(EntryTemplate template) {
		File file = new File(dirTemplate, template.getUuid() + ".yml");
		if (file.exists()) {
			file.delete();
		}
	}
}
