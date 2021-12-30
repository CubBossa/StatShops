package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import org.jetbrains.annotations.Nullable;

public class CloseModule extends BaseModule implements EntryModule {

	public CloseModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry) {
		super(provider, shopEntry);
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[]{};
	}

	@Override
	public void loadData() {
	}

	@Override
	public void saveData() {
	}

	@Override
	public EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {
		customer.getPlayer().closeInventory();
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		return null;
	}

	@Override
	public EntryModule duplicate() {
		return new CloseModule(provider, shopEntry);
	}
}
