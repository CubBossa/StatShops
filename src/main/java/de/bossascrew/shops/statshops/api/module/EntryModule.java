package de.bossascrew.shops.statshops.api.module;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.DataSlot;
import net.kyori.adventure.text.ComponentLike;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public interface EntryModule extends Duplicable<EntryModule>, ConfigurationSerializable {

	ShopEntry getShopEntry();

	void setShopEntry(ShopEntry entry);

	EntryModuleHandler.EntryModuleProvider getProvider();

	ComponentLike getDisplayName();

	ItemStack getDisplayItem();

	DataSlot<?>[] getDataSlots();

	void loadData();

	EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType);

	@Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result);
}
