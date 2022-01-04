package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.data.DatabaseObject;
import de.bossascrew.shops.statshops.handler.CustomerHandler;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

@RequiredArgsConstructor
public class Customer implements DatabaseObject {

	@Getter
	private final UUID uuid;
	private final OfflinePlayer player;
	@Getter
	private final Map<UUID, Integer> rememberedShopPages;
	private final Map<String, Long> messageCache;
	private Audience audience;

	@Getter
	@Setter
	private @Nullable
	Shop activeShop = null;

	public Customer(OfflinePlayer player, Map<UUID, Integer> rememberedShopPages) {
		this.player = player;
		this.uuid = player.getUniqueId();
		this.rememberedShopPages = rememberedShopPages;
		this.messageCache = new HashMap<>();
	}

	public static Customer wrap(Player player) {
		return CustomerHandler.getInstance().getCustomer(player);
	}

	public Player getPlayer() {
		if (player.isOnline()) {
			return player.getPlayer();
		}
		throw new IllegalStateException("Player from Customer is requested but not online");
	}

	public int getPage(Shop shop, int fallback) {
		return rememberedShopPages.getOrDefault(shop.getUUID(), fallback);
	}

	public void setPage(Shop shop, int page) {
		this.rememberedShopPages.put(shop.getUUID(), page);
	}

	public void sendMessage(Message message) {
		sendMessage(message.getKey(), message.getTranslation(), StatShops.getInstance().getShopsConfig().getMessageCaching());
	}

	public void sendMessage(String key, Component component) {
		sendMessage(key, component, StatShops.getInstance().getShopsConfig().getMessageCaching());
	}

	public void sendMessage(Message message, int cooldown) {
		sendMessage(message.getKey(), message.getTranslation(), cooldown);
	}

	public void sendMessage(String key, Component component, int cooldown) {
		if (!this.player.isOnline()) {
			return;
		}
		if (cooldown > 0) {
			Long cache = messageCache.get(key);
			if (cache != null && System.currentTimeMillis() - cache < cooldown) {
				return;
			}
		}
		if (this.audience == null) {
			this.audience = StatShops.getInstance().getBukkitAudiences().player(player.getPlayer());
		}
		audience.sendMessage(component);
		messageCache.put(key, System.currentTimeMillis());
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveCustomer(this);
	}
}
