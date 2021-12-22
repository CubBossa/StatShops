package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import org.jetbrains.annotations.Nullable;

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
		shopSlot = shopEntry.getData(DataSlot.ShopSlot.class, "open_shop", () -> {
			return new DataSlot.ShopSlot("open_shop", Message.GUI_ENTRY_FUNCTION_OPENED_SHOP_NAME,
					Message.GUI_ENTRY_FUNCTION_OPENED_SHOP_LORE, shop);
		});
		shopSlot.setUpdateHandler(uuid -> shop = ShopHandler.getInstance().getShop(uuid));
	}

	@Override
	public void saveData() {
		shopEntry.storeData(shopSlot);
	}

	@Override
	public EntryInteractionResult perform(Customer customer, EntryInteractionType interactionType) {
		if (shop == null) {
			System.out.println("Fail unknown");
			return EntryInteractionResult.FAIL_UNKNOWN;
		}
		shop.open(customer);
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		return null;
	}

	@Override
	public EntryModule duplicate() {
		return new OpenShopModule(provider, shopEntry, shop);
	}
}
