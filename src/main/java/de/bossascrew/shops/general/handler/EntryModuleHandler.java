package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.general.PaginatedShop;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.PageModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.menu.ListMenuElement;
import de.bossascrew.shops.general.menu.ListMenuElementHolder;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.hook.VaultHook;
import de.bossascrew.shops.statshops.shop.currency.DynamicPrice;
import de.bossascrew.shops.statshops.shop.currency.SimplePrice;
import de.bossascrew.shops.statshops.shop.entry.CloseModule;
import de.bossascrew.shops.statshops.shop.entry.PageBaseModule;
import de.bossascrew.shops.statshops.shop.entry.TradeBaseModule;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

@Getter
public class EntryModuleHandler implements ListMenuElementHolder<EntryModuleHandler.EntryModuleProvider> {

	public static EntryModuleProvider STATIC_PROVIDER;
	public static EntryModuleProvider CLOSE_PROVIDER;
	public static EntryModuleProvider NEXT_PAGE_PROVIDER;
	public static EntryModuleProvider PREV_PAGE_PROVIDER;
	public static EntryModuleProvider EXACT_PAGE_PROVIDER;
	public static EntryModuleProvider TRADE_ITEM_ITEM_PROVIDER;
	public static EntryModuleProvider TRADE_ITEM_VAULT_PROVIDER;
	public static EntryModuleProvider TRADE_CMD_ITEM_PROVIDER;
	public static EntryModuleProvider TRADE_CMD_VAULT_PROVIDER;
	public static EntryModuleProvider TRADE_CONSOLE_CMD_PROVIDER;

	public static CloseModule closeShop(ShopEntry shopEntry) {
		return new CloseModule(CLOSE_PROVIDER, shopEntry);
	}

	public static PageModule openExactPage(ShopEntry shopEntry, int page) {
		PageBaseModule pageModule = new PageBaseModule(EXACT_PAGE_PROVIDER, shopEntry, (customer, se, integer) -> {
			if (se.getShop() instanceof PaginatedShop ps) {
				ps.open(customer, integer);
				return;
			}
			se.getShop().open(customer);
		});
		pageModule.setNewPage(page);
		return pageModule;
	}

	public static PageModule openNextPage(ShopEntry shopEntry, int page) {
		PageBaseModule pageModule = new PageBaseModule(NEXT_PAGE_PROVIDER, shopEntry, (customer, se, integer) -> {
			if (se.getShop() instanceof PaginatedShop ps) {
				ps.open(customer, integer);
				return;
			}
			se.getShop().open(customer);
		});
		pageModule.setNewPageRelative(page);
		return pageModule;
	}

	public static PageModule openPrevPage(ShopEntry shopEntry, int page) {
		PageBaseModule pageModule = new PageBaseModule(PREV_PAGE_PROVIDER, shopEntry, (customer, se, integer) -> {
			if (se.getShop() instanceof PaginatedShop ps) {
				ps.open(customer, integer);
				return;
			}
			se.getShop().open(customer);
		});
		pageModule.setNewPageRelative(-1 * page);
		return pageModule;
	}

	public static TradeModule<ItemStack, ItemStack> tradeItemItem(ShopEntry entry, ItemStack gain, ItemStack pay) {
		return new TradeBaseModule.TradeItemItem(entry, TRADE_ITEM_ITEM_PROVIDER,
				new SimplePrice<>(CurrencyHandler.CURRENCY_ITEM, pay.getAmount(), pay),
				new SimplePrice<>(CurrencyHandler.CURRENCY_ITEM, gain.getAmount(), gain));
	}

	public static TradeModule<Void, ItemStack> tradeItemMoney(ShopEntry entry, ItemStack article, double amount) {
		return new TradeBaseModule<>(entry, TRADE_ITEM_VAULT_PROVIDER,
				new SimplePrice<>(VaultHook.CURRENCY_VAULT, amount, null),
				new SimplePrice<>(CurrencyHandler.CURRENCY_ITEM, article.getAmount(), article));
	}

	public static TradeModule<Void, ItemStack> tradeItemMoney(ShopEntry entry, ItemStack article, String amount) {
		return new TradeBaseModule<>(entry, TRADE_ITEM_VAULT_PROVIDER,
				new DynamicPrice<>(VaultHook.CURRENCY_VAULT, amount, null),
				new SimplePrice<>(CurrencyHandler.CURRENCY_ITEM, article.getAmount(), article));
	}

	public static TradeModule<Void, String> tradeCmdMoney(ShopEntry entry, String cmd, int cmdAmount, double amount) {
		TradeModule<Void, String> tm = new TradeBaseModule<>(entry, TRADE_CMD_VAULT_PROVIDER,
				new SimplePrice<>(VaultHook.CURRENCY_VAULT, amount, null),
				new SimplePrice<>(CurrencyHandler.CURRENCY_COMMAND, cmdAmount, cmd));
		tm.setSellable(false);
		tm.setPurchasableStacked(false);
		return tm;
	}

	public static TradeModule<ItemStack, String> tradeCmdItem(ShopEntry entry, String cmd, int cmdAmount, ItemStack pay) {
		TradeModule<ItemStack, String> tm = new TradeBaseModule<>(entry, TRADE_CMD_ITEM_PROVIDER,
				new SimplePrice<>(CurrencyHandler.CURRENCY_ITEM, pay.getAmount(), pay),
				new SimplePrice<>(CurrencyHandler.CURRENCY_COMMAND, cmdAmount, cmd));
		tm.setSellable(false);
		tm.setPurchasableStacked(false);
		return tm;
	}

	public static SimplePrice<ItemStack> itemPrice(int amount, ItemStack stack) {
		return new SimplePrice<>(CurrencyHandler.CURRENCY_ITEM, amount, stack);
	}

	public static SimplePrice<Void> moneyPrice(double amount) {
		return new SimplePrice<>(VaultHook.CURRENCY_VAULT, amount, null);
	}

	//TODO andere defaults

	@Getter
	private static EntryModuleHandler instance;

	private final Map<String, EntryModuleProvider> entryModules;

	public EntryModuleHandler() {
		instance = this;
		entryModules = new LinkedHashMap<>();
	}

	public EntryModuleProvider registerEntryModule(String key, ItemStack itemStack, Message name, Message lore, Function<ShopEntry, EntryModule> moduleSupplier) {
		EntryModuleProvider provider = new EntryModuleProvider(key, itemStack, name, lore, moduleSupplier);
		entryModules.put(key, provider);
		return provider;
	}

	/**
	 * Removes the EntryModule from the EntryEditor Menu. Does not remove the module from each entry.
	 *
	 * @param key the EntryModule to remove from the EntryModule selection menu.
	 */
	public void unregisterEntryModule(String key) {
		entryModules.remove(key);
	}

	public void registerDefaults() {
		//TODO default werte config
		STATIC_PROVIDER = registerEntryModule("static", new ItemStack(Material.BLACK_STAINED_GLASS), Message.GUI_ENTRY_FUNCTION_STATIC_NAME,
				Message.GUI_ENTRY_FUNCTION_STATIC_LORE, shopEntry -> null);
		CLOSE_PROVIDER = registerEntryModule("close", new ItemStack(Material.SPRUCE_DOOR), Message.GUI_ENTRY_FUNCTION_CLOSE_NAME,
				Message.GUI_ENTRY_FUNCTION_CLOSE_LORE, EntryModuleHandler::closeShop);
		EXACT_PAGE_PROVIDER = registerEntryModule("exact_page", new ItemStack(Material.BOOK), Message.GUI_ENTRY_FUNCTION_EXACT_PAGE_NAME,
				Message.GUI_ENTRY_FUNCTION_EXACT_PAGE_LORE, shopEntry -> openExactPage(shopEntry, 1));
		NEXT_PAGE_PROVIDER = registerEntryModule("next_page", new ItemStack(Material.BOOK), Message.GUI_ENTRY_FUNCTION_NEXT_PAGE_NAME,
				Message.GUI_ENTRY_FUNCTION_NEXT_PAGE_LORE, shopEntry -> openNextPage(shopEntry, 1));
		PREV_PAGE_PROVIDER = registerEntryModule("prev_page", new ItemStack(Material.BOOK), Message.GUI_ENTRY_FUNCTION_PREV_PAGE_NAME,
				Message.GUI_ENTRY_FUNCTION_PREV_PAGE_LORE, shopEntry -> openPrevPage(shopEntry, 1));
		TRADE_ITEM_ITEM_PROVIDER = registerEntryModule("trade_item_item", new ItemStack(Material.EMERALD), Message.GUI_ENTRY_FUNCTION_TRADE_ITEM_ITEM_NAME,
				Message.GUI_ENTRY_FUNCTION_TRADE_ITEM_ITEM_LORE, shopEntry -> tradeItemItem(shopEntry, shopEntry.getDisplayItem(), new ItemStack(Material.EMERALD, 5)));
		TRADE_ITEM_VAULT_PROVIDER = registerEntryModule("trade_item_vault", new ItemStack(Material.GOLD_INGOT), Message.GUI_ENTRY_FUNCTION_TRADE_ITEM_VAULT_NAME,
				Message.GUI_ENTRY_FUNCTION_TRADE_ITEM_VAULT_LORE, shopEntry -> tradeItemMoney(shopEntry, shopEntry.getDisplayItem(), 10.));
		TRADE_CMD_ITEM_PROVIDER = registerEntryModule("trade_cmd_item", new ItemStack(Material.REDSTONE), Message.GUI_ENTRY_FUNCTION_TRADE_CMD_ITEM_NAME,
				Message.GUI_ENTRY_FUNCTION_TRADE_CMD_ITEM_LORE, shopEntry -> tradeCmdItem(shopEntry, "none", 1, new ItemStack(Material.EMERALD, 5)));
		TRADE_CMD_VAULT_PROVIDER = registerEntryModule("trade_cmd_vault", new ItemStack(Material.REDSTONE), Message.GUI_ENTRY_FUNCTION_TRADE_CMD_VAULT_NAME,
				Message.GUI_ENTRY_FUNCTION_TRADE_CMD_VAULT_LORE, shopEntry -> tradeCmdMoney(shopEntry, "none", 1, 10.));
	}

	@Override
	public List<EntryModuleProvider> getValues() {
		return entryModules.values().stream().toList();
	}

	public static class EntryModuleProvider implements ListMenuElement {
		@Getter
		private final String key;
		private final ItemStack itemStack;
		private final Message name;
		private final Message lore;
		private final Function<ShopEntry, EntryModule> moduleSupplier;

		public EntryModuleProvider(String key, ItemStack itemStack, Message name, Message lore, Function<ShopEntry, EntryModule> moduleSupplier) {
			this.key = key;
			this.itemStack = itemStack;
			this.name = name;
			this.lore = lore;
			this.moduleSupplier = moduleSupplier;
		}

		public EntryModule getModule(ShopEntry shopEntry) {
			return moduleSupplier.apply(shopEntry);
		}

		@Override
		public Component getName() {
			return name.getTranslation();
		}

		@Override
		public ItemStack getListDisplayItem() {
			return ItemStackUtils.createItemStack(itemStack.clone(), name, lore);
		}
	}
}
