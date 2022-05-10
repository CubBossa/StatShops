package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.Message;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.UUID;

@Getter
@Setter
@RequiredArgsConstructor
@AllArgsConstructor
public class ShopType<S extends Shop> {

    private final NamespacedKey key;
    private Class<S> shopClass;
    private Message iconName = new Message("shoptype.icon.name");
    private Message iconLore = new Message("shoptype.icon.lore");
    private Material iconType = Material.ZOMBIE_VILLAGER_SPAWN_EGG;
    private @Nullable String permission;

    public S supplyShop(UUID uuid, String nameFormat, Map<String, DataSlot<?>> data) {
        try {
            Constructor<S> constructor = shopClass.getConstructor(UUID.class, String.class);
            S shop = constructor.newInstance(uuid, nameFormat);
            shop.loadData(data);
            shop.setDisplayItem(new ItemStack(iconType));
            return shop;
        } catch (NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
            StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not create shop of type '" + key + "', no matching constructor in class " + shopClass.getName() + " found.");
        }
        return null;
    }
}
