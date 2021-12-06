package de.bossascrew.shops.shop;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.menu.ListMenuElement;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.Editable;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.UUID;

public interface Shop extends Taggable, Comparable<Shop>, Editable<Player>, ListMenuElement, DatabaseObject {

	/**
	 * @return The unique id for this shop. It also servers as a tag for the Taggable interface and allows to apply limits and discounts to all shopentries of this shop
	 */
	UUID getUUID();

	/**
	 * @return The name format in minimessage style
	 */
	String getNameFormat();

	/**
	 * Allows to change the name format. The default implementation of this interface likewise sets the name component and the plain text string.
	 *
	 * @param nameFormat The name format in minimessage style.
	 */
	void setNameFormat(String nameFormat);

	/**
	 * @return The name as styled kyori component
	 */
	Component getName();

	/**
	 * @return The bare text name without formats and colors
	 */
	String getNamePlain();

	Material getDisplayMaterial();

	void setDisplayMaterial(Material material);

	/**
	 * @return The permission node that allows customers to use this shop or null if no permission is set
	 */
	@Nullable String getPermission();

	void setPermission(@Nullable String permission);

	/**
	 * @return The amount of pages of this shop. It may be calculated from the highest slot index.
	 */
	int getPageCount();

	ShopEntry getEntry(ShopMode mode, int slot);

	ShopEntry getEntry(UUID uuid);

	ShopEntry getUnusedEntry(UUID uuid);

	List<ShopEntry> getEntries(ShopMode shopMode, int shopPage);

	ShopEntry createEntry(ItemStack displayItem, ShopMode shopMode, int slot);

	boolean moveEntry(ShopEntry entry, ShopMode shopMode, int slot);

	boolean deleteEntry(ShopMode shopMode, int slot);

	boolean deleteEntry(ShopEntry entry);

	boolean setEntryUnused(ShopEntry entry);

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

	int getDefaultShopPage();

	void setDefaultShopPage(int page);

	ShopMode getPreferredShopMode(Customer customer);

	/**
	 * @return true if the shop is currently enabled. Customers cannot use disabled shops
	 */
	boolean isEnabled();

	/**
	 * If set to false, all active customers will be kicked from the shop. Customers cannot use disabled shops.
	 */
	void setEnabled(boolean enabled);

	/**
	 * @return all customers that currently use this shop and have an open shop interface.
	 */
	List<Customer> getActiveCustomers();

	void applyTemplate(EntryTemplate template, ShopMode shopMode, int shopPage);

	@Nullable EntryTemplate getDefaultTemplate();

	void setDefaultTemplate(@Nullable EntryTemplate defaultTemplate);

	/**
	 * @param customer the customer to open this shop for.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer);

	boolean open(Customer customer, ContextConsumer<BackContext> backHandler);

	/**
	 * @param customer the customer to open this shop for.
	 * @param page     the page to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, int page);

	boolean open(Customer customer, int page, ContextConsumer<BackContext> backHandler);

	/**
	 * @param customer the customer to open this shop for.
	 * @param shopMode the mode to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, ShopMode shopMode);

	boolean open(Customer customer, ShopMode shopMode, ContextConsumer<BackContext> backHandler);

	/**
	 * @param customer the customer to open this shop for.
	 * @param page     the page to open this shop at.
	 * @param shopMode the mode to open this shop at.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer, int page, ShopMode shopMode);

	boolean open(Customer customer, int page, ShopMode shopMode, ContextConsumer<BackContext> backHandler);

	/**
	 * @param customer the customer to close this shop if he currently uses it.
	 * @return true, if the shop was successfully closed. false, if the customer was not using this shop.
	 */
	boolean close(Customer customer);

	/**
	 * closes the shop for all active customers, for example when setting enabled to false.
	 */
	void closeAll();
}
