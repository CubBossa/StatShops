package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.data.Database;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.DataSlot;
import de.bossascrew.shops.statshops.shop.ShopType;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.web.WebAccessable;
import de.cubbossa.menuframework.inventory.ListMenuSupplier;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.stream.Collectors;

public class ShopHandler implements
        ListMenuSupplier<Shop> {

    @Getter
    private static ShopHandler instance;

    @Getter
    private final Map<UUID, Shop> shopMap;
    @Getter
    private final List<ShopType<? extends Shop>> registeredShopTypes;

    public ShopHandler() {
        instance = this;

        this.shopMap = new HashMap<>();
        this.registeredShopTypes = new ArrayList<>();

        registerShopType(new ShopType<>(new NamespacedKey(StatShops.getInstance(), "chest_menu_shop"),
                ChestMenuShop.class, Message.SHOP_TYPE_CHEST_NAME, Message.SHOP_TYPE_CHEST_LORE,
                Material.CHEST, StatShops.PERM_SHOP_TYPE_ + "chest_menu_shop"));
        registerShopType(new ShopType<>(new NamespacedKey(StatShops.getInstance(), "villager_menu_shop"),
                VillagerShop.class, Message.SHOP_TYPE_VILLAGER_NAME, Message.SHOP_TYPE_VILLAGER_LORE,
                Material.VILLAGER_SPAWN_EGG, StatShops.PERM_SHOP_TYPE_ + "villager_menu_shop"));

        for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
            extension.registerShopTypes(this);
        }
    }

    public void registerShopType(ShopType<?> shopType) {
        try {
            shopType.getShopClass().getConstructor(UUID.class, String.class);
        } catch (NoSuchMethodException e) {
            StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not register ShopType '" + shopType.getKey() + "', no serialisation constructor found.");
            return;
        }
        if (registeredShopTypes.stream().anyMatch(t -> t.getKey().equals(shopType.getKey()))) {
            StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not register ShopType '" + shopType.getKey() + "', another type with this key has already been registered.");
            return;
        }
        this.registeredShopTypes.add(shopType);
    }

    public void loadShopsFromDatabase(Database database) {
        this.shopMap.putAll(database.loadShops());
    }

    public List<Shop> getShops() {
        return shopMap.values().stream().sorted().collect(Collectors.toList());
    }

    public Shop getShop(UUID uuid) {
        return shopMap.get(uuid);
    }

    public void addShop(Shop shop) {
        shopMap.put(shop.getUUID(), shop);
    }

    public boolean deleteShop(Shop shop) {
        shop.closeAll();
        StatShops.getInstance().getDatabase().deleteShop(shop);
        if (StatShops.getInstance().isCitizensInstalled()) {
            StatShops.getInstance().getCitizensHook().removeAllAssignments(shop);
        }
        return shopMap.remove(shop.getUUID()) != null;
    }

    public List<Shop> getValues() {
        return getShops();
    }

    public List<ShopType<?>> getShopTypes() {
        return registeredShopTypes;
    }

    public @Nullable <S extends Shop> ShopType<S> getShopType(NamespacedKey key) {
        return (ShopType<S>) registeredShopTypes.stream().filter(t -> t.getKey().equals(key)).findFirst().orElse(null);
    }

    @Override
    public Collection<Shop> getElements() {
        return getShops();
    }

    @Override
    public ItemStack getDisplayItem(Shop object) {
        return ItemStackUtils.createShopItemStack(object);
    }

    public Shop createDuplicate(Shop element) {
        Shop shop = create(element.getNameFormat(), element.getClass());
        shop.setDisplayItem(element.getDisplayItem());
        shop.setPermission(element.getPermission());
        if (element instanceof PaginatedShop ps) { //TODO pro shop einen copy constructor -> via reflection aufrufen
            ps.setDefaultShopPage(ps.getDefaultShopPage());
            ps.setPageRemembered(ps.isPageRemembered());
        }
        for (String tag : element.getTags()) {
            shop.addTag(tag);
        }
        for (Map.Entry<Integer, ShopEntry> entry : element.getEntries().entrySet()) {
            shop.addEntry(entry.getValue().duplicate(), entry.getKey());
        }
        StatShops.getInstance().getDatabase().saveShop(shop);
        return shop;
    }
}
