package de.bossascrew.shops.shop;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import org.bukkit.inventory.ItemStack;

import java.util.List;

public interface PaginatedModedShop extends ModedShop, PaginatedShop {

	ShopEntry getEntry(ShopMode mode, int slot);

	List<ShopEntry> getEntries(ShopMode shopMode, int shopPage);

	ShopEntry createEntry(ItemStack displayItem, ShopMode shopMode, int slot);

	boolean moveEntry(ShopEntry entry, ShopMode shopMode, int slot);

	boolean deleteEntry(ShopMode shopMode, int slot);

	void applyTemplate(EntryTemplate template, ShopMode shopMode, int shopPage);

	/**
	 * @param customer the customer to open this shop for.
	 * @param page     the page to open this shop at.
	 * @param shopMode the mode to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, int page, ShopMode shopMode);

	boolean open(Customer customer, int page, ShopMode shopMode, ContextConsumer<BackContext> backHandler);
}
