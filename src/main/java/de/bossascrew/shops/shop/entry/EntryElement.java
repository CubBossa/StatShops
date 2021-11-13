package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.ShopInteractionResult;

public interface EntryElement {

	String getAmountDisplay();

	boolean canAct(Customer customer);

	ShopInteractionResult act(Customer customer);
}
