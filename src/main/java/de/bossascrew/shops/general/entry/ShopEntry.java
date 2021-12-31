package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.Taggable;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.DatabaseObject;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.entry.DataSlot;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Supplier;

public interface ShopEntry extends Taggable, Duplicable<ShopEntry>, DatabaseObject, Editable<Player> {

	UUID getUUID();

	Shop getShop();

	void setShop(Shop shop);

	int getSlot();

	void setSlot(int slot);

	ItemStack getDisplayItem();

	String getInfoLoreFormat();

	void setInfoLoreFormat(String lore);

	@Nullable String getPermission();

	void setPermission(@Nullable String permission);

	boolean hasPermission(Customer customer);

	Map<String, DataSlot<?>> getData();

	<T extends DataSlot<?>> T getData(Class<T> clazz, String key, Supplier<T> fallbackValue);

	@Nullable EntryModule getModule();

	void setModule(EntryModule module);

	EntryInteractionResult interact(Customer customer, ShopMenu menu, EntryInteractionType interactionType);

	List<String> getTags(boolean generated);
}
