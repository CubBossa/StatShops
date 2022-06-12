package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.shop.entry.ArticleSubModule;
import de.bossascrew.shops.statshops.shop.entry.CostsSubModule;
import de.bossascrew.shops.statshops.shop.entry.TradeBaseModule;
import de.cubbossa.translations.Message;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

public class SubModulesHandler {

	public static ArticleSubModuleProvider<ItemStack> ARTICLE_ITEM_PROVIDER;
	public static ArticleSubModuleProvider<String> ARTICLE_CMD_PROVIDER;
	public static ArticleSubModuleProvider<String> ARTICLE_CONSOLE_CMD_PROVIDER;

	public static CostsSubModuleProvider<ItemStack> COSTS_ITEM_PROVIDER;
	public static CostsSubModuleProvider<Void> COSTS_XP_PROVIDER;

	@Getter
	private static SubModulesHandler instance;

	private final Map<String, ArticleSubModuleProvider<?>> articleSubModules;
	private final Map<String, CostsSubModuleProvider<?>> costsSubModules;

	public SubModulesHandler() {
		instance = this;

		articleSubModules = new LinkedHashMap<>();
		costsSubModules = new LinkedHashMap<>();
	}

	public CostsSubModuleProvider<?> getCostsProvider(String key) {
		return costsSubModules.get(key);
	}

	public ArticleSubModuleProvider<?> getArticleProvider(String key) {
		return articleSubModules.get(key);
	}

	public void registerDefaults() {

		COSTS_ITEM_PROVIDER = registerCostsSubModule("item", StatShops.PERM_COSTS_ITEM, new ItemStack(Material.EMERALD), Messages.GUI_ENTRY_FUNCTION_COSTS_ITEM_NAME,
				Messages.GUI_ENTRY_FUNCTION_COSTS_ITEM_LORE, (provider, shopEntry) -> new CostsSubModule.ItemCosts(provider));
		COSTS_XP_PROVIDER = registerCostsSubModule("exp", StatShops.PERM_COSTS_XP, new ItemStack(Material.EXPERIENCE_BOTTLE), Messages.GUI_ENTRY_FUNCTION_COSTS_XP_NAME,
				Messages.GUI_ENTRY_FUNCTION_COSTS_XP_LORE, (provider, shopEntry) -> new CostsSubModule.ExpCosts(provider));

		ARTICLE_ITEM_PROVIDER = registerArticleSubModule(EntryModuleHandler.TRADE_ITEM, StatShops.PERM_ARTICLE_TRADE_ITEM, new ItemStack(Material.AZURE_BLUET), Messages.GUI_ENTRY_FUNCTION_ARTICLE_ITEM_NAME,
				Messages.GUI_ENTRY_FUNCTION_ARTICLE_ITEM_LORE, (provider, shopEntry) -> new ArticleSubModule.ItemArticle(provider));
		ARTICLE_CMD_PROVIDER = registerArticleSubModule(EntryModuleHandler.TRADE_CMD, StatShops.PERM_ARTICLE_TRADE_CMD, new ItemStack(Material.REDSTONE), Messages.GUI_ENTRY_FUNCTION_ARTICLE_CMD_NAME,
				Messages.GUI_ENTRY_FUNCTION_ARTICLE_CMD_LORE, (provider, shopEntry) -> new ArticleSubModule.CommandArticle(provider));
		ARTICLE_CONSOLE_CMD_PROVIDER = registerArticleSubModule(EntryModuleHandler.TRADE_CONSOLE_CMD, StatShops.PERM_ARTICLE_TRADE_CONSOLE_CMD, new ItemStack(Material.REPEATER), Messages.GUI_ENTRY_FUNCTION_ARTICLE_CONSOLE_CMD_NAME,
				Messages.GUI_ENTRY_FUNCTION_ARTICLE_CONSOLE_CMD_LORE, (provider, shopEntry) -> new ArticleSubModule.ConsoleCommandArticle(provider));

		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.registerTradeSubModules(this);
		}
	}

	public <A> ArticleSubModuleProvider<A> registerArticleSubModule(String key, @Nullable String permission, ItemStack itemStack, Message name, Message lore, BiFunction<ArticleSubModuleProvider<A>, ShopEntry, ArticleSubModule<A>> moduleSupplier) {
		ArticleSubModuleProvider<A> provider = new ArticleSubModuleProvider<>(key, itemStack, name, lore, moduleSupplier);
		articleSubModules.put(key, provider);

		EntryModuleHandler.EntryModuleProvider entryModuleProvider = EntryModuleHandler.getInstance().registerEntryModule(key, itemStack, name, lore, (p, entry) -> {
			return new TradeBaseModule(entry, p, provider.getModule(entry), COSTS_ITEM_PROVIDER.getModule(entry));
		});
		entryModuleProvider.setPermission(permission);
		return provider;
	}

	public <A> CostsSubModuleProvider<A> registerCostsSubModule(String key, @Nullable String permission, ItemStack itemStack, Message name, Message lore, BiFunction<CostsSubModuleProvider<A>, ShopEntry, CostsSubModule<A>> moduleSupplier) {
		CostsSubModuleProvider<A> provider = new CostsSubModuleProvider<>(key, itemStack, name, lore, moduleSupplier);
		provider.setPermission(permission);
		costsSubModules.put(key, provider);
		return provider;
	}

	public List<CostsSubModuleProvider<?>> getValues() {
		return new ArrayList<>(costsSubModules.values());
	}

	public static class ArticleSubModuleProvider<T> {
		@Getter
		private final String key;
		private final ItemStack itemStack;
		private final Message name;
		private final Message lore;
		private final BiFunction<ArticleSubModuleProvider<T>, ShopEntry, ArticleSubModule<T>> moduleSupplier;

		public ArticleSubModuleProvider(String key, ItemStack itemStack, Message name, Message lore, BiFunction<ArticleSubModuleProvider<T>, ShopEntry, ArticleSubModule<T>> moduleSupplier) {
			this.key = key;
			this.itemStack = itemStack;
			this.name = name;
			this.lore = lore;
			this.moduleSupplier = moduleSupplier;
		}

		public ArticleSubModule<T> getModule(ShopEntry shopEntry) {
			ArticleSubModule<T> subModule = moduleSupplier.apply(this, shopEntry);
			subModule.loadDataSlots(shopEntry);
			return subModule;
		}

		public ItemStack getDisplayItem() {
			return ItemStackUtils.createItemStack(itemStack.clone(), name, lore);
		}
	}

	public static class CostsSubModuleProvider<T> {
		@Getter
		private final String key;
		@Getter
		@Setter
		private @Nullable String permission = null;
		private final ItemStack itemStack;
		private final Message name;
		private final Message lore;
		private final BiFunction<CostsSubModuleProvider<T>, ShopEntry, CostsSubModule<T>> moduleSupplier;

		public CostsSubModuleProvider(String key, ItemStack itemStack, Message name, Message lore, BiFunction<CostsSubModuleProvider<T>, ShopEntry, CostsSubModule<T>> moduleSupplier) {
			this.key = key;
			this.itemStack = itemStack;
			this.name = name;
			this.lore = lore;
			this.moduleSupplier = moduleSupplier;
		}

		public CostsSubModule<T> getModule(ShopEntry shopEntry) {
			CostsSubModule<T> subModule = moduleSupplier.apply(this, shopEntry);

			subModule.loadDataSlots(shopEntry);
			return subModule;
		}

		public ItemStack getListDisplayItem() {
			return ItemStackUtils.createItemStack(itemStack.clone(), name, lore);
		}
	}
}
