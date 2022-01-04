package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.Cancellable;

@Getter
@Setter
public class ShopOpenEvent extends ShopEvent implements Cancellable {

	private final Customer customer;
	private int page;
	private boolean cancelled = false;

	public ShopOpenEvent(PaginatedShop shop, Customer customer, int page) {
		super(shop);
		this.customer = customer;
		this.page = page;
	}
}
