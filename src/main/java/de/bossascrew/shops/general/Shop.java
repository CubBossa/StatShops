package de.bossascrew.shops.general;

import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.statshops.data.DatabaseObject;
import de.bossascrew.shops.general.menu.ListMenuElement;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.util.Editable;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Map;
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

	Map<Integer, ShopEntry> getEntries();

	ShopEntry createEntry(ItemStack displayItem, int slot);

	ShopEntry addEntry(ShopEntry entry, int slot);

	boolean moveEntry(ShopEntry entry, int slot);

	ShopEntry getEntry(UUID uuid);

	ShopEntry getUnusedEntry(UUID uuid);

	boolean deleteEntry(ShopEntry entry);

	boolean removeEntry(ShopEntry entry);

	boolean deleteEntry(int slot);

	boolean setEntryUnused(ShopEntry entry);

	/**
	 * @return all customers that currently use this shop and have an open shop interface.
	 */
	List<Customer> getActiveCustomers();

	void applyTemplate(EntryTemplate template);

	@Nullable EntryTemplate getDefaultTemplate();

	void setDefaultTemplate(@Nullable EntryTemplate defaultTemplate);

	/**
	 * @param customer the customer to open this shop for.
	 * @return true if the shop was opened successfully, false if errors occured
	 */
	boolean open(Customer customer);

	boolean open(Customer customer, ContextConsumer<CloseContext> closeHandler);

	/**
	 * @param customer the customer to close this shop if he currently uses it.
	 * @return true, if the shop was successfully closed. false, if the customer was not using this shop.
	 */
	boolean close(Customer customer);

	/**
	 * closes the shop for all active customers, for example when setting enabled to false.
	 */
	void closeAll();

	void openEditorMenu(Player player, ContextConsumer<BackContext> backHandler);
}
