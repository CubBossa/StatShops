package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.general.PaginatedShop;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Database;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.jetbrains.annotations.Nullable;

import java.util.*;
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
		Shop shop = StatShops.getInstance().getDatabase().createShop(nameFormat, UUID.randomUUID());
		if (shop == null) {
			return null;
		}
		if (shop instanceof ChestMenuShop chestMenuShop) {
			chestMenuShop.setRows(StatShops.getInstance().getShopsConfig().getDefaultShopSize());
		}
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
		shop.setDisplayMaterial(element.getDisplayMaterial());
		shop.setPermission(element.getPermission());
		if (element instanceof PaginatedShop ps) { //TODO natürlich sollte das eignetlich jeder shop typ liefern
			ps.setDefaultShopPage(ps.getDefaultShopPage());
			ps.setPageRemembered(ps.isPageRemembered());
		}
		for (String tag : element.getTags()) {
			shop.addTag(tag);
		}
		//TODO clone all entries
		StatShops.getInstance().getDatabase().saveShop(shop);
		return null;
	}

	@Override
	public boolean delete(Shop element) {
		return deleteShop(element);
	}
}
