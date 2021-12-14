package de.bossascrew.shops.general.util;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Config;
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

	public static EntryInteractionType fromClickType(ClickType clickType) {
		Config sc = StatShops.getInstance().getShopsConfig();
		if (sc.getBuyKeyBinding().contains(clickType.toString())) {
			return BUY;
		} else if (sc.getBuyStackKeyBinding().contains(clickType.toString())) {
			return BUY_STACK;
		} else if (sc.getSellKeyBinding().contains(clickType.toString())) {
			return SELL;
		} else if (sc.getSellStackKeyBinding().contains(clickType.toString())) {
			return SELL_STACK;
		}
		return UNKNOWN;
	}
}
