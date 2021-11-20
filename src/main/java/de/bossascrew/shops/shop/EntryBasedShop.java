package de.bossascrew.shops.shop;

import de.bossascrew.shops.shop.entry.ShopEntry;
import org.bukkit.inventory.ItemStack;

import java.util.List;
import java.util.UUID;

public interface EntryBasedShop extends Shop {

	ShopEntry getEntry(ShopMode mode, int slot);

	ShopEntry getEntry(UUID uuid);

	ShopEntry getUnusedEntry(UUID uuid);

	List<ShopEntry> getEntries(ShopMode shopMode, int shopPage);

	ShopEntry createEntry(ItemStack displayItem, ShopMode shopMode, int slot);

	boolean moveEntry(ShopEntry entry, ShopMode shopMode, int slot);

	boolean deleteEntry(ShopMode shopMode, int slot);

	boolean deleteEntry(ShopEntry entry);

	boolean setEntryUnused(ShopEntry entry);
}
