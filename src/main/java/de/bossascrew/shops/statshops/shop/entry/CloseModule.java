package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.shop.DataSlot;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.Map;

public class CloseModule extends BaseModule implements EntryModule {

	public CloseModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry) {
		super(provider, shopEntry);
	}

	/**
	 * deserialize constructor. Provide shop entry afterwards!
	 */
	public CloseModule(Map<String, Object> values) {
		super(EntryModuleHandler.getInstance().getProvider((String) values.get("provider")), null);
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[]{};
	}

	@Override
	public void loadData() {
	}

	@Override
	public EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {
		customer.getPlayer().closeInventory();
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		if (!StatShops.getInstance().getShopsConfig().isLogModuleClose()) {
			return null;
		}
		if (result != EntryInteractionResult.SUCCESS) {
			return null;
		}
		return new LogEntry("customer: '" + customer.getUuid().toString() +
				"', entry: '" + shopEntry.getUUID().toString() +
				"', type: 'close shop', time: '" + LocalDateTime.now() + "'");
	}

	@Override
	public EntryModule duplicate() {
		return new CloseModule(provider, shopEntry);
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		return Map.of("provider", provider.getKey());
	}
}
