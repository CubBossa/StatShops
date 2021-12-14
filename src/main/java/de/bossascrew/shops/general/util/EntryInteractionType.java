package de.bossascrew.shops.general.util;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bukkit.event.inventory.ClickType;

@Getter
@RequiredArgsConstructor
public enum EntryInteractionType {

	BUY(true),
	SELL(false),
	BUY_STACK(true),
	SELL_STACK(false);

	private final boolean buy;

	public boolean isSell() {
		return !buy;
	}

	public static EntryInteractionType fromClickType(ClickType clickType) {
		return switch (clickType) { //TODO config parsen
			case RIGHT -> EntryInteractionType.SELL;
			case SHIFT_LEFT -> BUY_STACK;
			case SHIFT_RIGHT -> SELL_STACK;
			default -> BUY;
		};
	}
}
