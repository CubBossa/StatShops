package de.bossascrew.shops.handler;

import com.google.common.collect.Maps;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.OpenableMenu;
import de.bossascrew.shops.menu.VillagerMenu;
import lombok.Getter;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.TradeSelectEvent;

import java.util.Map;
import java.util.UUID;

public class InventoryHandler {

	@Getter
	private static InventoryHandler instance;

	@Getter
	private static ShopPlugin plugin;

	@Getter
	private final Map<UUID, OpenableMenu> openOpenableMenus = Maps.newHashMap();


	public InventoryHandler(ShopPlugin plugin) {
		InventoryHandler.plugin = plugin;
		instance = this;
	}

	public void handleMenuOpen(Player player, OpenableMenu menu) {
		handleInventoryClose(player);
		this.openOpenableMenus.put(player.getUniqueId(), menu);
	}

	public void handleInventoryClick(Player player, InventoryClickEvent event) {
		UUID playerId = player.getUniqueId();
		OpenableMenu invMenu = openOpenableMenus.get(playerId);

		if (invMenu != null) {
			invMenu.handleAction(player, event);
		}
	}

	public void handleInventoryDrag(Player player, InventoryDragEvent event) {
		UUID playerId = player.getUniqueId();
		OpenableMenu invMenu = openOpenableMenus.get(playerId);

		if (invMenu != null) {
			invMenu.handleAction(player, event);
		}
	}

	public void handleInventoryClose(Player player) {
		UUID playerId = player.getUniqueId();
		OpenableMenu invMenu = openOpenableMenus.remove(playerId);

		if (invMenu != null) {
			invMenu.closeInventory(player);
		}
	}

	public void handleTradeSelect(TradeSelectEvent event) {
		if (event.getWhoClicked() instanceof Player player) {

			UUID playerId = player.getUniqueId();
			OpenableMenu invMenu = openOpenableMenus.get(playerId);

			if (invMenu instanceof VillagerMenu villagerMenu) {
				villagerMenu.handleTradeSelect((Player) event.getWhoClicked(), event.getInventory().getSelectedRecipeIndex());
			}
		}
	}

	public void closeAllMenus() {
		for (OpenableMenu menu : openOpenableMenus.values()) {
			for (UUID uuid : menu.getOpenInventories().keySet()) {
				Player player = Bukkit.getPlayer(uuid);
				if (player == null) {
					continue;
				}
				player.closeInventory();
			}
		}
	}
}
