package de.bossascrew.shops.statshops.api;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import net.kyori.adventure.text.Component;

import java.util.List;

public interface PaginatedShop extends Shop, TemplatableShop {


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

	Component getPageTitle(int page);

	void setPageTitle(int page, String titleFormat);

	List<ShopEntry> getEntries(int page);

	void applyTemplate(EntryTemplate template, int shopPage);

	void applyDefaultTemplate(EntryTemplate template, int shopPage);

	/**
	 * @param customer the customer to open this shop for.
	 * @param page     the page to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, int page);
}
