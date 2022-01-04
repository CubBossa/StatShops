package de.bossascrew.shops.statshops.api;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.general.menu.ListMenuElement;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.statshops.api.data.DatabaseObject;
import de.bossascrew.shops.statshops.api.data.DisplayedObject;
import de.bossascrew.shops.statshops.api.data.NamedObject;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public interface Shop extends Taggable, Comparable<Shop>, Editable<Player>, ListMenuElement, DatabaseObject, NamedObject, DisplayedObject {

	/**
	 * @return The unique id for this shop. It also servers as a tag for the Taggable interface and allows applying limits
	 * and discounts to all shop entries of this shop.
	 */
	UUID getUUID();

	/**
	 * @return The permission node that allows customers to use this shop or null if no permission is set.
	 */
	@Nullable String getPermission();

	/**
	 * @param permission the new permission string for the shop or null, if no permission is required.
	 */
	void setPermission(@Nullable String permission);

	/**
	 * @return a {@link Map} of all active entries that are contained in this shop. This does not include
	 * unused entries that were temporarily removed from this shop. Use {@link #getUnusedEntry(UUID)} or {@link #getUnusedEntries()}
	 * to get all unused entries instead.
	 * <p>
	 * The entries are mapped to their slots. In case of a paginated shop, slots might be higher than the maximal slot count.
	 * <p>
	 * slot % maxPageSize = pageSlot
	 */
	Map<Integer, ShopEntry> getEntries();

	/**
	 * @param uuid the uuid of the {@link ShopEntry}
	 * @return the entry instance or null if no active entry with this {@link UUID} was found.
	 */
	ShopEntry getEntry(UUID uuid);

	/**
	 * Creates a new ShopEntry at the provided slot.
	 * The {@link de.bossascrew.shops.statshops.api.module.EntryModule} of this entry will be static, the {@link UUID}
	 * random and the permission String null. The display item of the entry will be the provided {@link ItemStack}
	 *
	 * @param displayItem the display item for the {@link ShopEntry}. See {@link DisplayedObject#setDisplayItem(ItemStack)}
	 * @param slot        the slot of the new {@link ShopEntry}. In case of pagination, this slot might be larger than the shop size.
	 * @return the new {@link ShopEntry} instance that was created or null, if errors occurred.
	 */
	ShopEntry createEntry(ItemStack displayItem, int slot);

	/**
	 * Adds a {@link ShopEntry} to the shop. If another {@link ShopEntry} already exists on that given slot, it will be deleted
	 * and the new {@link ShopEntry} will be inserted.
	 *
	 * @param entry The {@link ShopEntry} to insert into the shop.
	 * @param slot  The slot at which the Entry should be inserted.
	 * @return the {@link ShopEntry} that was inserted.
	 */
	ShopEntry addEntry(ShopEntry entry, int slot);

	/**
	 * Moves an existing Entry of that Shop from one slot to another.
	 *
	 * @param entry the {@link ShopEntry} to move.
	 * @param slot  the slot to move the {@link ShopEntry} to.
	 * @return true, if the {@link ShopEntry} has overridden another {@link ShopEntry} that was on the provided slot.
	 */
	boolean moveEntry(ShopEntry entry, int slot);

	/**
	 * Removes a {@link ShopEntry} and sets its slot to -1, but does not remove it from the UUID mapping.
	 * This {@link ShopEntry} will still be contained in the {@link #getEntries()} method but will not be rendered.
	 *
	 * @param entry the {@link ShopEntry} to remove from the shop.
	 * @return true, if the {@link ShopEntry} was successfully removed.
	 */
	boolean removeEntry(ShopEntry entry);

	/**
	 * Removes and deletes a {@link ShopEntry} from this shop.
	 *
	 * @param slot the slot to get the ShopEntry from.
	 * @return false if the {@link ShopEntry} could not be deleted or no {@link ShopEntry} existed on this slot. Otherwise true.
	 */
	boolean deleteEntry(int slot);

	/**
	 * Removes and deletes a {@link ShopEntry} from this shop.
	 *
	 * @param entry the {@link ShopEntry} to delete.
	 * @return false if the {@link ShopEntry} was not contained in the shop or could not be deleted. Otherwise true.
	 */
	boolean deleteEntry(ShopEntry entry);

	/**
	 * Get all {@link ShopEntry} objects from this Shop that are not contained in the slot mapping. These entries will not be
	 * rendered in shop menus. This can for example be used to allow users to move shop entries from one slot to another. If they
	 * put the shop entry item (that contains a UUID nbt tag) into their own inventory and reopen the editor, they can still
	 * place the item into the shop, and it will be loaded with all data that was applied to the unused entry.
	 *
	 * @return a collection of all unused entries.
	 */
	Collection<ShopEntry> getUnusedEntries();

	/**
	 * @param uuid the {@link UUID} of the {@link ShopEntry} that is supposed to be returned.
	 * @return an unused entry or null, of no unused entry with this {@link UUID} exists.
	 */
	ShopEntry getUnusedEntry(UUID uuid);

	/**
	 * Marks a {@link ShopEntry} that is contained in the slot mapping as unused and sets its slot to -1.
	 * This {@link ShopEntry} is still part of the shop but will not be rendered in a shop menu.
	 *
	 * @param entry the {@link ShopEntry} to mark as unused.
	 * @return true if the shop entry existed in the slot mapping and was successfully marked unused. Otherwise false.
	 */
	boolean setEntryUnused(ShopEntry entry);

	/**
	 * @return all customers that currently use this shop and have an open shop menu.
	 */
	List<Customer> getActiveCustomers();

	/**
	 * Opens the shop menu for the specified customer if the customer has permission to open this shop and the shop is not
	 * being edited at the moment.
	 *
	 * @param customer the customer to open this shop for.
	 * @return true if the shop was opened successfully, false if errors occurred
	 */
	boolean open(Customer customer);

	/**
	 * Opens the shop menu for the specified customer if the customer has permission to open this shop and the shop is not
	 * being edited at the moment.
	 *
	 * @param customer     The customer to open the shop for.
	 * @param closeHandler The CloseHandler that will be executed once the player closes the shop menu.
	 * @return true if the shop was opened successfully, false if errors occurred
	 */
	boolean open(Customer customer, ContextConsumer<CloseContext> closeHandler);

	/**
	 * Closes the shop menu for a specified customer. This will simulate the closing of the inventory by the player.
	 *
	 * @param customer the customer to close this shop if he is currently using this shop.
	 * @return true, if the shop was successfully closed. false, if the customer was not using this shop.
	 */
	boolean close(Customer customer);

	/**
	 * Closes the shop for all active customers.
	 */
	void closeAll();

	/**
	 * Opens the editor menu for administrators. This can be ignored if the shop implementation servers only internal purposes.
	 *
	 * @param player      the administrator that used the Shop Editor GUI to edit this shop instance.
	 * @param backHandler the BackHandler which reopens the Shops List Menu. If not called, administrators have to reopen the
	 *                    editor, so consider adding a "back" button.
	 */
	void openEditorMenu(Player player, ContextConsumer<BackContext> backHandler);
}
