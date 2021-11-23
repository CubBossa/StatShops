package de.bossascrew.shops.hook;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.ShopHandler;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import net.citizensnpcs.api.event.NPCRightClickEvent;
import net.citizensnpcs.api.trait.Trait;
import net.citizensnpcs.api.util.DataKey;
import org.bukkit.event.EventHandler;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public class ShopTrait extends Trait {

	@Getter
	private Shop shop;

	protected ShopTrait(Shop shop) {
		super(CitizensHook.TRAIT_NAME);
		this.shop = shop;
	}

	public void setShop(@Nullable Shop shop) {
		this.shop = shop;
	}

	public void load(DataKey key) {
		String uuidString = key.getString("shop-uuid");
		if (uuidString == null) {
			return;
		}
		try {
			UUID uuid = UUID.fromString(uuidString);
			this.shop = ShopHandler.getInstance().getShop(uuid);
		} catch (IllegalArgumentException e) {
			ShopPlugin.getInstance().log(LoggingPolicy.DEBUG, "Loaded citizens statshop trait without a valid shop set.");
		}
	}

	public void save(DataKey key) {
		key.setString("shop-uuid", shop == null ? "null" : shop.getUUID().toString());
	}

	@EventHandler
	public void click(NPCRightClickEvent event) {
		if (shop == null) {
			ShopPlugin.getInstance().log(LoggingPolicy.WARN, "Shoptrait assigned to #" + npc.getId() + " but shop is null.");
			return;
		}
		if (event.getNPC() == this.getNPC()) {
			shop.open(Customer.wrap(event.getClicker()));
		}
	}

	@Override
	public void onAttach() {
		ShopPlugin.getInstance().log(LoggingPolicy.DEBUG, npc.getName() + " has been assigned statshop");
	}
}