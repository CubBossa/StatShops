package de.bossascrew.shops.shop;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;

public interface PaginatedShop extends Shop {


	/**
	 * @return The amount of pages of this shop. It may be calculated from the highest slot index.
	 */
	int getPageCount();

	/**
	 * @return true, if customers open the shop at the same page they have closed it
	 */
	boolean isPageRemembered();

	/**
	 * @param rememberPage If set to true, customers open this shop at the page they have closed it
	 */
	void setPageRemembered(boolean rememberPage);

	/**
	 * @return The page to open the shop at for a certain customer
	 */
	int getPreferredOpenPage(Customer customer);

	int getDefaultShopPage();

	void setDefaultShopPage(int page);

	void applyTemplate(EntryTemplate template, int shopPage);

	/**
	 * @param customer the customer to open this shop for.
	 * @param page     the page to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, int page);

	boolean open(Customer customer, int page, ContextConsumer<BackContext> backHandler);

	/**
	 * @param customer the customer to open this shop for.
	 * @param page     the page to open this shop at.
	 * @param shopMode the mode to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, int page, ShopMode shopMode);

	boolean open(Customer customer, int page, ShopMode shopMode, ContextConsumer<BackContext> backHandler);
}
