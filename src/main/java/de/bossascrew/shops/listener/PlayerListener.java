package de.bossascrew.shops.listener;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.CustomerHandler;
import de.bossascrew.shops.handler.InventoryHandler;
import lombok.RequiredArgsConstructor;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryCloseEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerQuitEvent;

@RequiredArgsConstructor
public class PlayerListener implements Listener {

	private final ShopPlugin shopPlugin;

	@EventHandler
	public void onJoin(PlayerJoinEvent event) {
		if (ShopPlugin.getInstance().isLoading()) {
			event.getPlayer().kickPlayer("Server is still starting");
			return;
		}
		CustomerHandler.getInstance().getCustomer(event.getPlayer());
	}

	@EventHandler
	public void onQuit(PlayerQuitEvent event) {
		InventoryHandler.getInstance().handleInventoryClose(event.getPlayer());
	}

	@EventHandler
	public void onInventoryClick(InventoryClickEvent event) {
		if (shopPlugin.isLoading()) {
			return;
		}
		if (event.getWhoClicked() instanceof Player player) {
			InventoryHandler.getInstance().handleInventoryClick(player, event);
		}
	}

	@EventHandler
	public void onInventoryDrag(InventoryDragEvent event) {
		if(shopPlugin.isLoading()) {
			return;
		}
		if (event.getWhoClicked() instanceof Player player) {
			InventoryHandler.getInstance().handleInventoryDrag(player, event);
		}
	}

	@EventHandler
	public void onInventoryClose(InventoryCloseEvent event) {
		if(shopPlugin.isLoading()) {
			return;
		}
		if (event.getPlayer() instanceof Player player) {
			InventoryHandler.getInstance().handleInventoryClose(player);
		}
	}
}
