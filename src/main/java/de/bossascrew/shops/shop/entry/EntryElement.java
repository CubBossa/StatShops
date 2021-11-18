package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.util.Duplicable;

public interface EntryElement extends Duplicable<EntryElement> {

	String getAmountDisplay();

	boolean canAct(Customer customer);

	ShopInteractionResult act(Customer customer);
}
