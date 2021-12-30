package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.entry.DataSlot;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public interface EntryModule extends Duplicable<EntryModule> {

	ShopEntry getShopEntry();

	void setShopEntry(ShopEntry entry);

	EntryModuleHandler.EntryModuleProvider getProvider();

	Component getDisplayName();

	ItemStack getDisplayItem();

	DataSlot<?>[] getDataSlots();

	void loadData();

	void saveData();

	EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType);

	@Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result);
}
