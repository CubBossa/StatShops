package de.bossascrew.shops.handler;

import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.ListMenuElement;
import de.bossascrew.shops.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.menu.ListMenuElementHolder;
import de.bossascrew.shops.shop.entry.EntryModule;
import de.bossascrew.shops.shop.entry.PageBaseModule;
import de.bossascrew.shops.shop.entry.PageModule;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

@Getter
public class EntryModuleHandler implements ListMenuElementHolder<EntryModuleHandler.EntryModuleProvider> {

	public static PageModule openExactPage(ShopEntry shopEntry, int page) {
		PageBaseModule pageModule = new PageBaseModule(shopEntry, (customer, integer) -> shopEntry.getShop().open(customer, integer));
		pageModule.setNewPage(page);
		return pageModule;
	}

	public static PageModule openNextPage(ShopEntry shopEntry, int page) {
		PageBaseModule pageModule = new PageBaseModule(shopEntry, (customer, integer) -> shopEntry.getShop().open(customer, integer));
		pageModule.setNewPageRelative(page);
		return pageModule;
	}

	public static PageModule openPrevPage(ShopEntry shopEntry, int page) {
		PageBaseModule pageModule = new PageBaseModule(shopEntry, (customer, integer) -> shopEntry.getShop().open(customer, integer));
		pageModule.setNewPageRelative(-1 * page);
		return pageModule;
	}
	//TODO andere defaults

	@Getter
	private static EntryModuleHandler instance;

	private final Map<String, EntryModuleProvider> entryModules;

	public EntryModuleHandler() {
		instance = this;
		entryModules = new HashMap<>();
	}

	public void registerEntryModule(String key, ItemStack itemStack, Message name, Message lore, Function<ShopEntry, EntryModule> moduleSupplier) {
		entryModules.put(key, new EntryModuleProvider(key, itemStack, name, lore, moduleSupplier));
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
		//TODO messages
		registerEntryModule("exact_page", new ItemStack(Material.BOOK), null, null, shopEntry -> openExactPage(shopEntry, 1));
		registerEntryModule("next_page", new ItemStack(Material.BOOK), null, null, shopEntry -> openExactPage(shopEntry, 1));
		registerEntryModule("prev_page", new ItemStack(Material.BOOK), null, null, shopEntry -> openExactPage(shopEntry, 1));
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
			return ItemStackUtils.createItemStack(itemStack, name, lore);
		}
	}
}
