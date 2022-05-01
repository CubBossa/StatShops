package de.bossascrew.shops.statshops.menu;

import de.cubbossa.guiframework.inventory.Action;
import de.cubbossa.guiframework.inventory.MenuPresets;
import de.cubbossa.guiframework.inventory.context.ClickContext;
import de.cubbossa.guiframework.inventory.context.ContextConsumer;
import de.cubbossa.guiframework.inventory.implementations.RectInventoryMenu;
import net.kyori.adventure.text.ComponentLike;

public class ConfirmMenu extends RectInventoryMenu {

	public ConfirmMenu(ComponentLike title) {
		super(title, 3);
		addPreset(MenuPresets.fill(Icon.EMPTY_DARK_RP.create()));
	}

	public void setAcceptHandler(ContextConsumer<ClickContext> acceptHandler) {
		setItemAndClickHandler(9 + 2, Icon.ACCEPT_RP.create(), Action.LEFT, acceptHandler);
	}

	public void setDenyHandler(ContextConsumer<ClickContext> denyHandler) {
		setItemAndClickHandler(9 + 6, Icon.DECLINE_RP.create(), Action.LEFT, denyHandler);
	}
}

