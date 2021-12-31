package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;

public class OpenShopModule extends BaseModule implements EntryModule {

	private Shop shop;
	private DataSlot.ShopSlot shopSlot;

	public OpenShopModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry, Shop shop) {
		super(provider, shopEntry);
		this.shop = shop;

		loadData();
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[] {shopSlot};
	}

	@Override
	public void loadData() {
		shopSlot = shopEntry.getData(DataSlot.ShopSlot.class, "open_shop", () -> new DataSlot.ShopSlot(shop));
		shopSlot.setUpdateHandler(uuid -> shop = ShopHandler.getInstance().getShop(uuid));
	}

	@Override
	public EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {
		if (shop == null) {
			return EntryInteractionResult.FAIL_UNKNOWN;
		}
		shop.open(customer);
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		if (!StatShops.getInstance().getShopsConfig().isLogModuleOpenOtherShop()) {
			return null;
		}
		if (result != EntryInteractionResult.SUCCESS) {
			return null;
		}
		return new LogEntry("customer: '" + customer.getUuid().toString() +
				"', entry: '" + shopEntry.getUUID().toString() +
				"', type: 'open shop', time: '" + LocalDateTime.now() +
				"', shop: '" + shop.getUUID().toString() + "'");
	}

	@Override
	public EntryModule duplicate() {
		return new OpenShopModule(provider, shopEntry, shop);
	}
}
