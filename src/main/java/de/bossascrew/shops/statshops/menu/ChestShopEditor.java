package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;

import java.util.*;

public class ChestShopEditor {

	private final ChestMenuShop shop;
	private final Map<Integer, ChestShopPageEditor> map;
	private final ContextConsumer<BackContext> backHandler;
	private final ContextConsumer<CloseContext> paginatedCloseContext;
	private final Collection<UUID> expectedPageTurns;

	@Getter
	@Setter
	private boolean freezeItems = true;

	public ChestShopEditor(ChestMenuShop shop, ContextConsumer<BackContext> backHandler, ContextConsumer<CloseContext> paginatedCloseContext) {
		this.shop = shop;
		this.map = new HashMap<>();
		this.backHandler = backHandler;
		this.paginatedCloseContext = paginatedCloseContext;
		this.expectedPageTurns = new HashSet<>();
	}

	public void registerPageTurn(Player player) {
		expectedPageTurns.add(player.getUniqueId());
	}

	public void handleClose(Player player, CloseContext closeContext) {
		if (!expectedPageTurns.contains(player.getUniqueId())) {
			try {
				paginatedCloseContext.accept(closeContext);
			} catch (Throwable t) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Error while handling editor close: ", t);
			}
		}
	}

	public void openInventory(Player player, int shopPage) {
		expectedPageTurns.remove(player.getUniqueId());
		ChestShopPageEditor pageMenu = get(shopPage);
		if (pageMenu == null) {
			pageMenu = new ChestShopPageEditor(shop, shopPage, backHandler, this);
			map.put(shopPage, pageMenu);
		}
		pageMenu.openInventory(player);
	}

	public ChestShopPageEditor get(int shopPage) {
		return map.get(shopPage);
	}
}
