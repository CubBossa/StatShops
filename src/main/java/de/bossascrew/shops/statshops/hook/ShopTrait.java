package de.bossascrew.shops.statshops.hook;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import lombok.Getter;
import net.citizensnpcs.api.event.NPCRightClickEvent;
import net.citizensnpcs.api.trait.Trait;
import net.citizensnpcs.api.util.DataKey;
import org.bukkit.event.EventHandler;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

public class ShopTrait extends Trait {

	@Getter
	private @Nullable Shop shop;

	public ShopTrait() {
		super(CitizensHook.TRAIT_NAME);
		this.shop = null;
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
			StatShops.getInstance().log(LoggingPolicy.DEBUG, "Loaded citizens statshop trait without a valid shop set.");
		}
	}

	public void save(DataKey key) {
		key.setString("shop-uuid", shop == null ? "null" : shop.getUUID().toString());
	}

	@EventHandler
	public void click(NPCRightClickEvent event) {
		if (shop == null) {
			StatShops.getInstance().log(LoggingPolicy.WARN, "Shoptrait assigned to " + npc.getName() + " (#" + npc.getId() + ") but shop is null. Use \"/npc select " +
					npc.getId() + "\" and \"/trait remove statshop\"");
			return;
		}
		if (event.getNPC() == this.getNPC()) {
			shop.open(Customer.wrap(event.getClicker()));
		}
	}

	@Override
	public void onAttach() {
		StatShops.getInstance().log(LoggingPolicy.DEBUG, npc.getName() + " has been assigned statshop");
	}
}