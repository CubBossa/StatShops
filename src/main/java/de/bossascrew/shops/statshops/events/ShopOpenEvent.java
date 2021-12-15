package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.Cancellable;

@Getter
@Setter
public class ShopOpenEvent extends ShopEvent implements Cancellable {

	private final Customer customer;
	private int page;
	private boolean cancelled = false;

	public ShopOpenEvent(Shop shop, Customer customer, int page) { //TODO eigentlich ist page shop spezifisch
		super(shop);
		this.customer = customer;
		this.page = page;
	}
}
