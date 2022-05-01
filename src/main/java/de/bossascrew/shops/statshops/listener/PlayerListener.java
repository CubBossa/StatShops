package de.bossascrew.shops.statshops.listener;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.handler.CustomerHandler;
import lombok.RequiredArgsConstructor;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.TradeSelectEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerQuitEvent;

@RequiredArgsConstructor
public class PlayerListener implements Listener {

	private final StatShops shopPlugin;

	@EventHandler
	public void onJoin(PlayerJoinEvent event) {
		if (StatShops.busy()) {
			event.getPlayer().kickPlayer("Server is still starting");
			return;
		}
		CustomerHandler.getInstance().getCustomer(event.getPlayer());
	}
}
