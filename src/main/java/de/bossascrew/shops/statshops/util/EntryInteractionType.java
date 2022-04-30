package de.bossascrew.shops.statshops.util;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Config;
import de.cubbossa.guiframework.inventory.Action;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bukkit.event.inventory.ClickType;

@Getter
@RequiredArgsConstructor
public enum EntryInteractionType {

	BUY(true, false),
	SELL(false, true),
	BUY_STACK(true, false),
	SELL_STACK(false, true),
	UNKNOWN(false, false);

	private final boolean buy;
	private final boolean sell;

	public static EntryInteractionType fromAction(Action<?> action) {
		String a = action.toString();
		Config sc = StatShops.getInstance().getShopsConfig();
		if (sc.getBuyKeyBinding().contains(a)) {
			return BUY;
		} else if (sc.getBuyStackKeyBinding().contains(a)) {
			return BUY_STACK;
		} else if (sc.getSellKeyBinding().contains(a)) {
			return SELL;
		} else if (sc.getSellStackKeyBinding().contains(a)) {
			return SELL_STACK;
		}
		return UNKNOWN;
	}
}
