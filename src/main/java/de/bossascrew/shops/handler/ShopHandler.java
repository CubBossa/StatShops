package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.util.WebSessionInterface;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

import java.util.*;

public class ShopHandler implements WebSessionInterface {

	@Getter
	private static ShopHandler instance;

	@Getter
	private final Map<UUID, Shop> shopMap;
	private ShopMode headShopMode = null;
	private ShopMode tailShopMode = null;

	public ShopHandler() {
		instance = this;

		this.shopMap = new HashMap<>();
	}

	public List<Shop> getShops() {
		return shopMap.values().stream().toList();
	}

	public Shop createShop(String nameFormat) {
		Shop shop = ShopPlugin.getInstance().getDatabase().createShop(nameFormat, UUID.randomUUID());
		shop.setDefaultShopMode(headShopMode);
		addShop(shop);
		return shop;
	}

	public void addShop(Shop shop) {
		shopMap.put(shop.getUUID(), shop);
	}

	public void deleteShop(Shop shop) {
		shop.closeAll();
		ShopPlugin.getInstance().getDatabase().deleteShop();
	}

	public List<ShopMode> getShopModes() {
		List<ShopMode> shopModes = new ArrayList<>();
		ShopMode temp = headShopMode;
		while (temp.getNext() != null) {
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
		headShopMode.setPrevious(shopMode);
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
	public String toJson() {
		return "";
	}
}
