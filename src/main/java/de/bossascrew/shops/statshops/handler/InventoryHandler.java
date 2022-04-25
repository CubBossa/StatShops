package de.bossascrew.shops.statshops.handler;

import com.google.common.collect.Maps;
import de.bossascrew.shops.general.menu.EditorMenu;
import de.bossascrew.shops.general.menu.OpenableMenu;
import de.bossascrew.shops.general.menu.VillagerMenu;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Message;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
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
	private static StatShops plugin;

	@Getter
	private final Map<UUID, OpenableMenu> openOpenableMenus = Maps.newHashMap();


	public InventoryHandler(StatShops plugin) {
		InventoryHandler.plugin = plugin;
		instance = this;
	}

	public boolean canMenuOpen(Player player, OpenableMenu menu) {
		if(menu instanceof EditorMenu editorMenu) {

			if (!editorMenu.isEditorSet() || editorMenu.getEditor().equals(player)) {
				return true;
			} else {
				Customer.wrap(player).sendMessage(Message.GENERAL_EDITABLE_CURRENTLY_EDITED.getKey(),
						Message.GENERAL_EDITABLE_CURRENTLY_EDITED.getTranslation(TagResolver.resolver("editor",
								Tag.inserting(Component.text(((Player) editorMenu.getEditor()).getName())))));
				return false;
			}
		}
		return true;
	}

	public void handleMenuOpen(Player player, OpenableMenu menu) {
		if(menu instanceof EditorMenu editorMenu) {
			editorMenu.setEditor(player);
		}
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
			if(invMenu instanceof EditorMenu editorMenu) {
				editorMenu.setEditor(null);
			}
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

	/**
	 * @param simulateEvent when stopping the server (onDisable), there will be called no InventoryCloseEvent. Menus therefore will not
	 *                      call their close method with close handler automatically. Setting simulateEvent to true will call
	 *                      the close method manually. If set to false, the close method will only be called if an event
	 *                      is called. Setting it to true when a close event is called will lead to two calls of the menu close method.
	 */
	public void closeAllMenus(boolean simulateEvent) {
		for (OpenableMenu menu : openOpenableMenus.values()) {
			for (UUID uuid : menu.getOpenInventories().keySet()) {
				Player player = Bukkit.getPlayer(uuid);
				if (player == null) {
					continue;
				}
				player.closeInventory();
				if (simulateEvent) {
					handleInventoryClose(player);
				}
			}
		}
	}
}
