package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.Cancellable;

@Getter
@Setter
public class ShopTurnPageEvent extends ShopEvent implements Cancellable {

	private final Customer customer;
	private int newPage;
	private boolean cancelled;

	public ShopTurnPageEvent(PaginatedShop shop, Customer customer, int newPage) {
		super(shop);
		this.customer = customer;
		this.newPage = newPage;
	}
}
