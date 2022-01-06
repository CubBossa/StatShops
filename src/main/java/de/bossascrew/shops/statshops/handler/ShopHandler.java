package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.data.Database;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

public class ShopHandler implements
		WebAccessable<Shop>,
		ListManagementMenuElementHolder<Shop> {

	@Getter
	private static ShopHandler instance;

	@Getter
	private final Map<UUID, Shop> shopMap;

	public ShopHandler() {
		instance = this;

		this.shopMap = new HashMap<>();
	}

	public void loadShopsFromDatabase(Database database) {
		this.shopMap.putAll(database.loadShops());
	}

	public List<Shop> getShops() {
		return shopMap.values().stream().sorted().collect(Collectors.toList());
	}

	public Shop getShop(UUID uuid) {
		return shopMap.get(uuid);
	}

	public @Nullable
	Shop createShop(String nameFormat) {
		ChestMenuShop shop = new ChestMenuShop(UUID.randomUUID(), nameFormat); //TODO typen registrieren
		shop.setRows(StatShops.getInstance().getShopsConfig().getDefaultShopSize());
		StatShops.getInstance().getDatabase().saveShop(shop);
		addShop(shop);
		return shop;
	}

	public void addShop(Shop shop) {
		shopMap.put(shop.getUUID(), shop);
	}

	public boolean deleteShop(Shop shop) {
		shop.closeAll();
		StatShops.getInstance().getDatabase().deleteShop(shop);
		if (StatShops.getInstance().isCitizensInstalled()) {
			StatShops.getInstance().getCitizensHook().removeAllAssignments(shop);
		}
		return shopMap.remove(shop.getUUID()) != null;
	}

	@Override
	public List<Shop> getWebData() {
		return getShops();
	}

	@Override
	public void storeWebData(List<Shop> values) {
		//TODO
	}

	@Override
	public List<Shop> getValues() {
		return getShops();
	}

	@Override
	public Shop createNew(String input) {
		return createShop(input);
	}

	@Override
	public Shop createDuplicate(Shop element) {
		Shop shop = createShop(element.getNameFormat());
		shop.setDisplayItem(element.getDisplayItem());
		shop.setPermission(element.getPermission());
		if (element instanceof PaginatedShop ps) { //TODO pro shop einen copy constructor -> via reflection aufrufen
			ps.setDefaultShopPage(ps.getDefaultShopPage());
			ps.setPageRemembered(ps.isPageRemembered());
		}
		for (String tag : element.getTags()) {
			shop.addTag(tag);
		}
		for (Map.Entry<Integer, ShopEntry> entry : element.getEntries().entrySet()) {
			shop.addEntry(entry.getValue().duplicate(), entry.getKey());
		}
		StatShops.getInstance().getDatabase().saveShop(shop);
		return shop;
	}

	@Override
	public boolean delete(Shop element) {
		return deleteShop(element);
	}
}
