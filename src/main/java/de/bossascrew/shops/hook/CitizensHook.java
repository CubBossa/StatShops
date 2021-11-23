package de.bossascrew.shops.hook;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.shop.Shop;
import net.citizensnpcs.api.CitizensAPI;
import net.citizensnpcs.api.event.NPCClickEvent;
import net.citizensnpcs.api.npc.NPC;
import net.citizensnpcs.api.trait.TraitInfo;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class CitizensHook implements Listener {

	public static final String TRAIT_NAME = "statshop";

	private final Map<Player, Shop> assigningPlayers;
	private final Map<Player, Shop> confirmingPlayers;

	public CitizensHook(ShopPlugin shopPlugin) {

		CitizensAPI.getTraitFactory().registerTrait(TraitInfo.create(ShopTrait.class).withName(TRAIT_NAME));
		assigningPlayers = new HashMap<>();
		confirmingPlayers = new HashMap<>();

		Bukkit.getPluginManager().registerEvents(this, shopPlugin);
	}

	@EventHandler
	public void onLeftClick(PlayerInteractEvent event) {
		if (event.getAction().equals(Action.LEFT_CLICK_AIR) || event.getAction().equals(Action.LEFT_CLICK_BLOCK)) {
			assigningPlayers.remove(event.getPlayer());
		}
	}

	@EventHandler
	public void onNpcClick(NPCClickEvent event) {

		Player player = event.getClicker();
		Customer customer = Customer.wrap(player);
		Shop shop = assigningPlayers.remove(player);
		if (shop != null) {
			if (event.getNPC().hasTrait(ShopTrait.class)) {
				confirmingPlayers.put(player, shop);
				customer.sendMessage(Message.CITIZENS_CONFIRM_OVERRIDE);
				return;
			}
			event.getNPC().addTrait(new ShopTrait(shop));
		} else {
			shop = confirmingPlayers.remove(player);
			if (shop == null) {
				return;
			}
			event.getNPC().getTrait(ShopTrait.class).setShop(shop);
		}
		customer.sendMessage(Message.CITIZENS_ASSIGNED);
	}

	public void addAssigningPlayer(Player player, Shop shop) {
		this.assigningPlayers.put(player, shop);
	}

	public void removeAllAssignments(Shop shop) {
		for (NPC npc : CitizensAPI.getNPCRegistry()) {
			ShopTrait t = npc.getTrait(ShopTrait.class);
			if(t != null && t.getShop().getUUID().equals(shop.getUUID())) {
				npc.removeTrait(ShopTrait.class);
			}
		}
	}
}
