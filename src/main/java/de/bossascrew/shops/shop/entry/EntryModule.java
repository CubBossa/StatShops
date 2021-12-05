package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.LogEntry;
import de.bossascrew.shops.shop.ShopInteractionResult;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public interface EntryModule {

	ItemStack getDisplayItem();

	void loadData();

	void saveData();

	@Nullable LogEntry createLogEntry();

	ShopInteractionResult perform(Customer customer);
}
