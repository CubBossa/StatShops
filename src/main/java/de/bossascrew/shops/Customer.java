package de.bossascrew.shops;

import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.CustomerHandler;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@RequiredArgsConstructor
public class Customer implements DatabaseObject {

	@Getter
	private final UUID uuid;
	@Getter
	private final Player player;
	private final Audience audience;
	private final Map<Shop, Integer> rememberedShopPages;
	private final Map<Shop, ShopMode> rememberedShopModes;
	private final Map<String, Long> messageCache;

	@Getter
	@Setter
	private @Nullable
	Shop activeShop = null;

	public Customer(Player player, Map<Shop, Integer> rememberedShopPages, Map<Shop, ShopMode> rememberedShopModes) {
		this.player = player;
		this.uuid = player.getUniqueId();
		this.audience = ShopPlugin.getInstance().getBukkitAudiences().player(player);
		this.rememberedShopModes = rememberedShopModes;
		this.rememberedShopPages = rememberedShopPages;
		this.messageCache = new HashMap<>();
	}

	public static Customer wrap(Player player) {
		return CustomerHandler.getInstance().getCustomer(player);
	}

	public int getPage(Shop shop, int fallback) {
		return rememberedShopPages.getOrDefault(shop, fallback);
	}

	public @Nullable
	ShopMode getShopMode(Shop shop, ShopMode fallback) {
		return rememberedShopModes.getOrDefault(shop, fallback);
	}

	public void setPage(Shop shop, int page) {
		this.rememberedShopPages.put(shop, page);
	}

	public void setMode(Shop shop, ShopMode shopMode) {
		this.rememberedShopModes.put(shop, shopMode);
	}

	public void sendMessage(Message message) {
		sendMessage(message.getKey(), message.getTranslation(), ShopPlugin.getInstance().getShopsConfig().getMessageCaching());
	}

	public void sendMessage(String key, Component component) {
		sendMessage(key, component, ShopPlugin.getInstance().getShopsConfig().getMessageCaching());
	}

	public void sendMessage(Message message, int cooldown) {
		sendMessage(message.getKey(), message.getTranslation(), cooldown);
	}

	public void sendMessage(String key, Component component, int cooldown) {
		if (cooldown > 0) {
			Long cache = messageCache.get(key);
			if (cache != null && System.currentTimeMillis() - cache < cooldown) {
				return;
			}
		}
		audience.sendMessage(component);
		messageCache.put(key, System.currentTimeMillis());
	}

	@Override
	public void saveToDatabase() {
		ShopPlugin.getInstance().getDatabase().saveCustomer(this);
	}
}
