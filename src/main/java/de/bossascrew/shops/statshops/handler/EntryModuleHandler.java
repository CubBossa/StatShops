package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.shop.entry.CloseModule;
import de.bossascrew.shops.statshops.shop.entry.OpenShopModule;
import de.bossascrew.shops.statshops.shop.entry.PageBaseModule;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.translations.Message;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.ComponentLike;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

@Getter
public class EntryModuleHandler {

    public static EntryModuleProvider STATIC_PROVIDER;
    public static EntryModuleProvider CLOSE_PROVIDER;
    public static EntryModuleProvider OPEN_SHOP_PROVIDER;
    public static EntryModuleProvider NEXT_PAGE_PROVIDER;
    public static EntryModuleProvider PREV_PAGE_PROVIDER;
    public static EntryModuleProvider EXACT_PAGE_PROVIDER;

    public static final String TRADE_ITEM = "trade_item";
    public static final String TRADE_CMD = "trade_cmd";
    public static final String TRADE_CONSOLE_CMD = "trade_console_cmd";

    public static CloseModule closeShop(ShopEntry shopEntry) {
        return new CloseModule(CLOSE_PROVIDER, shopEntry);
    }

    public static OpenShopModule openShop(ShopEntry shopEntry, Shop shop) {
        OpenShopModule shopModule = new OpenShopModule(OPEN_SHOP_PROVIDER, shopEntry);
        shopModule.getShopSlot().setData(shop.getUUID());
        return shopModule;
    }

    public static PageBaseModule openExactPage(ShopEntry shopEntry, int page) {
        PageBaseModule pageModule = new PageBaseModule(EXACT_PAGE_PROVIDER, shopEntry);
        pageModule.setNewPage(page);
        return pageModule;
    }

    public static PageBaseModule openNextPage(ShopEntry shopEntry, int page) {
        PageBaseModule pageModule = new PageBaseModule(NEXT_PAGE_PROVIDER, shopEntry);
        pageModule.setNewPageRelative(page);
        return pageModule;
    }

    public static PageBaseModule openPrevPage(ShopEntry shopEntry, int page) {
        PageBaseModule pageModule = new PageBaseModule(PREV_PAGE_PROVIDER, shopEntry);
        pageModule.setNewPageRelative(-1 * page);
        return pageModule;
    }

    @Getter
    private static EntryModuleHandler instance;

    private final Map<String, EntryModuleProvider> entryModules;

    public EntryModuleHandler() {
        instance = this;
        entryModules = new LinkedHashMap<>();
    }

    public EntryModuleProvider getProvider(String name) {
        return entryModules.get(name);
    }

    public EntryModule getModule(ShopEntry shopEntry, String name) {
        return entryModules.get(name).getModule(shopEntry);
    }

    public EntryModuleProvider registerEntryModule(String key, ItemStack itemStack, Message name, Message lore, BiFunction<EntryModuleProvider, ShopEntry, EntryModule> moduleSupplier) {
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

        STATIC_PROVIDER = registerEntryModule("static", new ItemStack(Material.BLACK_STAINED_GLASS), Messages.GUI_ENTRY_FUNCTION_STATIC_NAME,
                Messages.GUI_ENTRY_FUNCTION_STATIC_LORE, (provider, shopEntry) -> null);
        STATIC_PROVIDER.setPermission(StatShops.PERM_ARTICLE_STATIC);
        CLOSE_PROVIDER = registerEntryModule("close", new ItemStack(Material.SPRUCE_DOOR), Messages.GUI_ENTRY_FUNCTION_CLOSE_NAME,
                Messages.GUI_ENTRY_FUNCTION_CLOSE_LORE, (provider, shopEntry) -> closeShop(shopEntry));
        CLOSE_PROVIDER.setPermission(StatShops.PERM_ARTICLE_CLOSE_SHOP);
        OPEN_SHOP_PROVIDER = registerEntryModule("open_shop", new ItemStack(Material.VILLAGER_SPAWN_EGG), Messages.GUI_ENTRY_FUNCTION_OPEN_SHOP_NAME,
                Messages.GUI_ENTRY_FUNCTION_OPEN_SHOP_LORE, (provider, shopEntry) -> openShop(shopEntry, shopEntry.getShop()));
        OPEN_SHOP_PROVIDER.setPermission(StatShops.PERM_ARTICLE_OPEN_SHOP);
        EXACT_PAGE_PROVIDER = registerEntryModule("exact_page", new ItemStack(Material.BOOK), Messages.GUI_ENTRY_FUNCTION_EXACT_PAGE_NAME,
                Messages.GUI_ENTRY_FUNCTION_EXACT_PAGE_LORE, (provider, shopEntry) -> openExactPage(shopEntry, 1));
        EXACT_PAGE_PROVIDER.setPermission(StatShops.PERM_ARTICLE_EXACT_PAGE);
        NEXT_PAGE_PROVIDER = registerEntryModule("next_page", new ItemStack(Material.BOOK), Messages.GUI_ENTRY_FUNCTION_NEXT_PAGE_NAME,
                Messages.GUI_ENTRY_FUNCTION_NEXT_PAGE_LORE, (provider, shopEntry) -> openNextPage(shopEntry, 1));
        NEXT_PAGE_PROVIDER.setPermission(StatShops.PERM_ARTICLE_NEXT_PAGE);
        PREV_PAGE_PROVIDER = registerEntryModule("prev_page", new ItemStack(Material.BOOK), Messages.GUI_ENTRY_FUNCTION_PREV_PAGE_NAME,
                Messages.GUI_ENTRY_FUNCTION_PREV_PAGE_LORE, (provider, shopEntry) -> openPrevPage(shopEntry, 1));
        PREV_PAGE_PROVIDER.setPermission(StatShops.PERM_ARTICLE_PREV_PAGE);

        // All trade modules are automatically registered by the submodules handler

        // External entries
        for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
            extension.registerEntryModules(this);
        }
    }

    public List<EntryModuleProvider> getValues() {
        return entryModules.values().stream().toList();
    }

    public static class EntryModuleProvider {
        @Getter
        private final String key;
        @Getter
        @Setter
        private @Nullable String permission = null;
        private final ItemStack itemStack;
        private final Message name;
        private final Message lore;
        private final BiFunction<EntryModuleProvider, ShopEntry, EntryModule> moduleSupplier;

        public EntryModuleProvider(String key, ItemStack itemStack, Message name, Message lore, BiFunction<EntryModuleProvider, ShopEntry, EntryModule> moduleSupplier) {
            this.key = key;
            this.itemStack = itemStack;
            this.name = name;
            this.lore = lore;
            this.moduleSupplier = moduleSupplier;
        }

        public ComponentLike getName() {
            return name;
        }

        public EntryModule getModule(ShopEntry shopEntry) {
            return moduleSupplier.apply(this, shopEntry);
        }

        public ItemStack getListDisplayItem() {
            return ItemStackUtils.createItemStack(itemStack.clone(), name, lore);
        }
    }
}
