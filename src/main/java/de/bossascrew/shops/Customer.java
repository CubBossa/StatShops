package de.bossascrew.shops;

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

import java.util.Map;
import java.util.UUID;

@RequiredArgsConstructor
public class Customer {

	@Getter
	private final UUID uuid;
	@Getter
	private final Player player;
	private final Audience audience;
	private final Map<Shop, Integer> rememberedShopPages;
	private final Map<Shop, ShopMode> rememberedShopModes;

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
		sendMessage(message.getTranslation());
	}

	public void sendMessage(Component component) {
		audience.sendMessage(component);
	}
}
