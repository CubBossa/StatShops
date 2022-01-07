package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import lombok.Getter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

public class OpenShopModule extends BaseModule implements EntryModule {

	@Getter
	private DataSlot.ShopSlot shopSlot;

	public OpenShopModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry) {
		super(provider, shopEntry);

		if (shopEntry != null) {
			loadData();
		}
	}

	/**
	 * deserialize constructor. Provide shop entry afterwards!
	 */
	public OpenShopModule(Map<String, Object> values) {
		super(EntryModuleHandler.getInstance().getProvider((String) values.get("provider")), null);
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[]{shopSlot};
	}

	@Override
	public void loadData() {
		shopSlot = shopEntry.getData(DataSlot.ShopSlot.class, "open_shop", () -> new DataSlot.ShopSlot(shopEntry.getShop().getUUID()));
	}

	@Override
	public EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {
		Shop shop = ShopHandler.getInstance().getShop(shopSlot.getData());
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
				"', shop: '" + shopSlot.getData() + "'");
	}

	@Override
	public EntryModule duplicate() {
		OpenShopModule shopModule = new OpenShopModule(provider, shopEntry);
		shopModule.shopSlot.setData(shopSlot.getData());
		return shopModule;
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		Map<String, Object> map = new HashMap<>();
		map.put("provider", provider.getKey());
		return map;
	}
}
