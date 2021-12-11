package de.bossascrew.shops.shop;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;

public interface ModedShop {

	/**
	 * @return true, if customers open the shop at the same shop mode they have closed it
	 */
	boolean isModeRemembered();

	/**
	 * @param rememberMode If set to true, customers open this shop at the shop mode they have closed it
	 */
	void setModeRemembered(boolean rememberMode);

	ShopMode getDefaultShopMode();

	void setDefaultShopMode(ShopMode shopMode);

	ShopMode getPreferredShopMode(Customer customer);

	void applyTemplate(EntryTemplate template, ShopMode shopMode);

	/**
	 * @param customer the customer to open this shop for.
	 * @param shopMode the mode to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, ShopMode shopMode);

	boolean open(Customer customer, ShopMode shopMode, ContextConsumer<BackContext> backHandler);
}