package de.bossascrew.shops.shop.entry;

import net.kyori.adventure.text.Component;

public interface PayElement extends EntryElement {

	/**
	 * @return the formatted costs of this element. Like "1x Diamond" or "37.5$"
	 */
	Component getPriceDisplay();
}
