package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.ShopInteractionResult;

public interface EntryElement {

	boolean canAct(Customer customer);

	ShopInteractionResult act(Customer customer);

	boolean demandsLogging();

	//TODO they need to load their data (what item to sell and the price eg from the entry storage)
}
