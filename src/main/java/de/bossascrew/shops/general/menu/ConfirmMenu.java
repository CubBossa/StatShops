package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.general.menu.ChestMenu;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ClickContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.TextUtils;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryType;
import org.jetbrains.annotations.Nullable;

import java.util.Map;

public class ConfirmMenu extends ChestMenu {

	@Getter
	private @Nullable
	ContextConsumer<ClickContext> acceptHandler;

	@Getter
	private @Nullable
	ContextConsumer<ClickContext> denyHandler;


	public ConfirmMenu(String title) {
		this(title, null, null, null);
	}

	public ConfirmMenu(Component title) {
		this(title, null, null, null);
	}

	public ConfirmMenu(String title, ContextConsumer<BackContext> backHandler) {
		this(title, null, null, backHandler);
	}

	public ConfirmMenu(Component title, ContextConsumer<BackContext> backHandler) {
		this(title, null, null, backHandler);
	}

	public ConfirmMenu(String title, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler,
					   @Nullable ContextConsumer<CloseContext> closeHandler, @Nullable ContextConsumer<BackContext> backHandler) {
		this(TextUtils.fromLegacy(title), defaultClickHandler, closeHandler, backHandler);
	}

	public ConfirmMenu(Component title, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler,
					   @Nullable ContextConsumer<CloseContext> closeHandler, @Nullable ContextConsumer<BackContext> backHandler) {
		super(title, InventoryType.CHEST, 3, 26, defaultClickHandler, closeHandler);
		this.backHandler = backHandler;
		fillMenu(backHandler);
	}

	public void setAcceptHandler(ContextConsumer<ClickContext> acceptHandler) {
		this.acceptHandler = acceptHandler;
		setItemAndClickHandler(1, 2, DefaultSpecialItem.ACCEPT.createSpecialItem(), ClickType.LEFT, acceptHandler);
	}

	public void setDenyHandler(ContextConsumer<ClickContext> denyHandler) {
		this.denyHandler = denyHandler;
		setItemAndClickHandler(1, 6, DefaultSpecialItem.DECLINE.createSpecialItem(), ClickType.LEFT, denyHandler);
	}
}

