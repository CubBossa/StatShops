package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.statshops.data.DatabaseObject;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import de.bossascrew.shops.statshops.shop.ShopMode;
import de.bossascrew.shops.general.Taggable;
import de.bossascrew.shops.general.util.Duplicable;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public interface ShopEntry extends Taggable, Duplicable<ShopEntry>, DatabaseObject, Editable<Player> {

	UUID getUUID();

	Shop getShop();

	void setShop(Shop shop);

	int getSlot();

	void setSlot(int slot);

	ShopMode getShopMode();

	void setShopMode(ShopMode shopMode);

	ItemStack getDisplayItem();

	String getInfoLoreFormat();

	void setInfoLoreFormat(String lore);

	@Nullable String getPermission();

	void setPermission(@Nullable String permission);

	boolean hasPermission(Customer customer);

	<T> T getData(Class<T> clazz, String key);

	<T> T storeData(Class<T> clazz, String key, T value);

	@Nullable EntryModule getModule();

	void setModule(EntryModule module);

	ShopInteractionResult interact(Customer customer);
}
