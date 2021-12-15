package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.util.EntryInteractionType;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.Cancellable;

@Getter
@Setter
public class ShopEntryInteractEvent extends ShopEntryEvent implements Cancellable {

	private final EntryInteractionType action;
	private final Customer customer;
	private boolean cancelled = false;

	public ShopEntryInteractEvent(ShopEntry shopEntry, Customer customer, EntryInteractionType action) {
		super(shopEntry);
		this.customer = customer;
		this.action = action;
	}
}
