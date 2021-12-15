package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.general.Shop;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.Cancellable;

public class ShopCloseEvent extends ShopEvent implements Cancellable {

	@Getter
	private final int currentPage;
	@Getter
	@Setter
	private boolean cancelled = false;

	public ShopCloseEvent(Shop shop, int currentPage) {
		super(shop);
		this.currentPage = currentPage;
	}
}
