package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import lombok.Getter;

@Getter
public class ShopEntryInteractedEvent extends ShopEntryEvent {

	private final Customer customer;
	private final EntryInteractionType action;
	private final EntryInteractionResult result;

	public ShopEntryInteractedEvent(ShopEntry shopEntry, Customer customer, EntryInteractionType action, EntryInteractionResult result) {
		super(shopEntry);
		this.customer = customer;
		this.action = action;
		this.result = result;
	}
}
