package de.bossascrew.shops.statshops.api;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.statshops.api.data.DatabaseObject;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.DataSlot;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Supplier;

public interface ShopEntry extends Taggable, Duplicable<ShopEntry>, DatabaseObject, Editable<Player>, DataSlotHolder {

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

	@Nullable EntryModule getModule();

	void setModule(EntryModule module);

	EntryInteractionResult interact(Customer customer, ShopMenu menu, EntryInteractionType interactionType);

	List<String> getTags(boolean generated);
}
