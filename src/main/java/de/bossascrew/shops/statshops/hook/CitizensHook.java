package de.bossascrew.shops.statshops.hook;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Messages;
import net.citizensnpcs.api.CitizensAPI;
import net.citizensnpcs.api.event.NPCRightClickEvent;
import net.citizensnpcs.api.npc.NPC;
import net.citizensnpcs.api.trait.TraitInfo;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.Event;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CitizensHook implements Listener {

	public static final String TRAIT_NAME = "statshop";

	private final Map<Player, Shop> assigningPlayers;
	private final Map<Player, Shop> confirmingPlayers;

	public CitizensHook(StatShops shopPlugin) {

		CitizensAPI.getTraitFactory().registerTrait(TraitInfo.create(ShopTrait.class).withName(TRAIT_NAME));
		assigningPlayers = new HashMap<>();
		confirmingPlayers = new HashMap<>();

		Bukkit.getPluginManager().registerEvents(this, shopPlugin);
	}

	public List<String> getAssignedNPCs(Shop shop) {
		List<String> result = new ArrayList<>();
		for (NPC npc : CitizensAPI.getNPCRegistry()) {
			if (!npc.hasTrait(ShopTrait.class)) {
				continue;
			}
			ShopTrait shopTrait = npc.getTrait(ShopTrait.class);
			if (shopTrait.getShop() == null) {
				continue;
			}
			if (shopTrait.getShop().getUUID().equals(shop.getUUID())) {
				result.add(npc.getName() + " (#" + npc.getId() + ")");
			}
		}
		return result;
	}

	@EventHandler
	public void onLeftClick(PlayerInteractEvent event) {
		if (event.getAction().equals(Action.LEFT_CLICK_AIR) || event.getAction().equals(Action.LEFT_CLICK_BLOCK)) {
			if (assigningPlayers.remove(event.getPlayer()) != null) {
				event.setUseItemInHand(Event.Result.DENY);
				event.setUseInteractedBlock(Event.Result.DENY);
				Customer.wrap(event.getPlayer()).sendMessage(Messages.CITIZENS_CANCELLED);
			}
		}
	}

	@EventHandler(priority = EventPriority.LOWEST)
	public void onNpcClick(NPCRightClickEvent event) {

		Player player = event.getClicker();
		Customer customer = Customer.wrap(player);
		Shop shop = assigningPlayers.remove(player);
		if (shop != null) {
			if (event.getNPC().hasTrait(ShopTrait.class)) {
				confirmingPlayers.put(player, shop);
				customer.sendMessage(Messages.CITIZENS_CONFIRM_OVERRIDE);
				return;
			}
			ShopTrait trait = new ShopTrait();
			trait.setShop(shop);
			event.getNPC().addTrait(trait);
			event.setCancelled(true);
		} else {
			shop = confirmingPlayers.remove(player);
			if (shop == null) {
				return;
			}
			event.setCancelled(true);
			event.getNPC().getTrait(ShopTrait.class).setShop(shop);
		}
		customer.sendMessage(Messages.CITIZENS_ASSIGNED);
	}

	public void addAssigningPlayer(Player player, Shop shop) {
		this.assigningPlayers.put(player, shop);
	}

	public void removeAllAssignments(Shop shop) {
		for (NPC npc : CitizensAPI.getNPCRegistry()) {
			ShopTrait t = npc.getTrait(ShopTrait.class);
			if(t != null && t.getShop() != null && t.getShop().getUUID().equals(shop.getUUID())) {
				npc.removeTrait(ShopTrait.class);
			}
		}
	}
}
