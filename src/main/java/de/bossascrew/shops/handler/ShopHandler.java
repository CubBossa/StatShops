package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class ShopHandler implements WebAccessable<Shop> {

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

	public List<Shop> getShops() {
		return shopMap.values().stream().sorted().collect(Collectors.toList());
	}

	public @Nullable
	Shop createShop(String nameFormat) {
		Shop shop = ShopPlugin.getInstance().getDatabase().createShop(nameFormat, UUID.randomUUID());
		if (shop == null) {
			return null;
		}
		shop.setDefaultShopMode(headShopMode);
		addShop(shop);
		return shop;
	}

	public void addShop(Shop shop) {
		shopMap.put(shop.getUUID(), shop);
	}

	public void deleteShop(Shop shop) {
		shop.closeAll();
		shopMap.remove(shop.getUUID());
		ShopPlugin.getInstance().getDatabase().deleteShop(shop);
	}

	public List<ShopMode> getShopModes() {
		List<ShopMode> shopModes = new ArrayList<>();
		ShopMode temp = headShopMode;
		while (!temp.getNext().equals(headShopMode)) {
			shopModes.add(temp);
			temp = temp.getNext();
		}
		return shopModes;
	}

	public void registerShopMode(ShopMode shopMode) {
		if (tailShopMode == null) {
			tailShopMode = shopMode;
			headShopMode = shopMode;
			return;
		}
		shopMode.setNext(headShopMode);
		shopMode.setPrevious(tailShopMode);
		headShopMode.setPrevious(shopMode);
		tailShopMode.setNext(shopMode);
		headShopMode = shopMode;
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
				return ItemStackUtils.createItemStack(ShopPlugin.getInstance().getShopsConfig().getShopBuyIconMaterial(), Message.SHOP_MODE_BUY_NAME, Message.SHOP_MODE_BUY_LORE);
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
				return ItemStackUtils.createItemStack(ShopPlugin.getInstance().getShopsConfig().getShopSellIconMaterial(), Message.SHOP_MODE_SELL_NAME, Message.SHOP_MODE_SELL_LORE);
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
}
