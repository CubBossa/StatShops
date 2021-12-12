package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Database;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.general.ModedShop;
import de.bossascrew.shops.general.PaginatedModedShop;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.ShopMode;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
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

	/**
	 * Cyclic data structure. shopMode.next() gives the next element and allows to iterate. If you need all ShopModes you can call getShopModes()
	 */
	private ShopMode headShopMode = null;
	private ShopMode tailShopMode = null;

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
		if (shop instanceof ModedShop ms) {
			ms.setDefaultShopMode(headShopMode);
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

	public List<ShopMode> getShopModes() {
		List<ShopMode> shopModes = new ArrayList<>();
		ShopMode temp = headShopMode;
		while (temp != null) {
			shopModes.add(temp);
			if (temp.getNext() != null && !temp.getNext().equals(headShopMode)) {
				temp = temp.getNext();
			} else {
				temp = null;
			}
		}
		return shopModes;
	}

	public void registerShopMode(ShopMode shopMode) {
		if (getShopModes().stream().anyMatch(sm -> sm.equals(shopMode))) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "A shopmode with the key " + shopMode.getKey() + " already exists.");
			return;
		}

		if (tailShopMode == null) {
			tailShopMode = shopMode;
		} else {
			shopMode.setNext(headShopMode);
			shopMode.setPrevious(tailShopMode);
			headShopMode.setPrevious(shopMode);
			tailShopMode.setNext(shopMode);
		}
		headShopMode = shopMode;
		StatShops.getInstance().log(LoggingPolicy.INFO, "ShopMode \"" + shopMode.getKey() + "\" registered successfully.");
	}

	public void registerDefaultShopModes() {
		registerShopMode(new ShopMode() {
			public String getKey() {
				return "BUY";
			}

			public Component getDisplayName() {
				return Message.SHOP_MODE_BUY_NAME.getTranslation();
			}

			public ItemStack getDisplayItem() {
				return ItemStackUtils.createItemStack(Material.DIAMOND, Message.SHOP_MODE_BUY_NAME, Message.SHOP_MODE_BUY_LORE);
			}
		});
		registerShopMode(new ShopMode() {
			public String getKey() {
				return "SELL";
			}

			public Component getDisplayName() {
				return Message.SHOP_MODE_SELL_NAME.getTranslation();
			}

			public ItemStack getDisplayItem() {
				return ItemStackUtils.createItemStack(Material.GOLD_INGOT, Message.SHOP_MODE_SELL_NAME, Message.SHOP_MODE_SELL_LORE);
			}
		});
		registerShopMode(new ShopMode() {
			public String getKey() {
				return "TRADE";
			}

			public Component getDisplayName() {
				return Message.SHOP_MODE_TRADE_NAME.getTranslation();
			}

			public ItemStack getDisplayItem() {
				return ItemStackUtils.createItemStack(Material.EMERALD, Message.SHOP_MODE_TRADE_NAME, Message.SHOP_MODE_TRADE_LORE);
			}
		});
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
		shop.setEnabled(element.isEnabled());
		if (element instanceof PaginatedModedShop ps) { //TODO natürlich sollte das eignetlich jeder shop typ liefern
			ps.setDefaultShopMode(ps.getDefaultShopMode());
			ps.setPageRemembered(ps.isPageRemembered());
			ps.setModeRemembered(ps.isModeRemembered());
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
