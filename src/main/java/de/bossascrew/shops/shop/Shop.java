package de.bossascrew.shops.shop;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.UUID;

public interface Shop extends Taggable, Comparable<Shop> {

	UUID getUUID();

	String getNameFormat();

	Component getName();

	@Nullable String getPermission();

	ShopEntry getEntry(ShopMode mode, int slot);

	boolean isPagingCyclic();

	void setPagingCyclic(boolean cyclic);

	boolean isPageRemembered();

	void setRememberPage(boolean rememberPage);

	int getPreferredOpenPage(Customer customer);

	boolean isModeRemembered();

	void setRememberMode(boolean rememberMode);

	ShopMode setDefaultShopMode(ShopMode shopMode);

	ShopMode getDefaultShopMode();

	ShopMode getPreferredShopMode(Customer customer);

	void setEnabled(boolean enabled);

	boolean isEnabled();

	List<Customer> getActiveCustomers();

	@Nullable Player getEditingPlayer();

	void setEditingPlayer(Player player);

	boolean open(Customer customer);

	boolean open(Customer customer, int page);

	boolean open(Customer customer, ShopMode shopMode);

	boolean open(Customer customer, int page, ShopMode shopMode);

	boolean close(Customer customer);

	void closeAll();

	ShopInteractionResult interact(Customer customer, ShopMode shopMode, int slot);
}
