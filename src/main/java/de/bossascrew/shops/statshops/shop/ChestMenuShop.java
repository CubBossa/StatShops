package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.api.TransactionBalanceMessenger;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.events.ShopCloseEvent;
import de.bossascrew.shops.statshops.events.ShopOpenEvent;
import de.bossascrew.shops.statshops.events.ShopTurnPageEvent;
import de.bossascrew.shops.statshops.menu.ChestShopEditor;
import de.bossascrew.shops.statshops.menu.ChestShopMenu;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.cubbossa.guiframework.inventory.Menu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

@Getter
@Setter
public class ChestMenuShop extends BaseShop implements PaginatedShop {

	private @Nullable EntryTemplate defaultTemplate = null;
	private TransactionBalanceMessenger balanceMessenger;
	private final List<UUID> pageTurningPlayers;

	private int rows = 3;

	private boolean isPageRemembered = false;
	private int defaultPage = 0;

	private final Map<Integer, String> pageTitles;

	public ChestMenuShop(String nameFormat) {
		this(UUID.randomUUID(), nameFormat);
	}

	public ChestMenuShop(UUID uuid, String nameFormat) {
		super(uuid, nameFormat);

		this.balanceMessenger = new SimpleBalanceMessenger(StatShops.getInstance().getShopsConfig().getTradeMessageFeedback());
		this.pageTurningPlayers = new ArrayList<>();
		this.pageTitles = new HashMap<>();

		this.rows = StatShops.getInstance().getShopsConfig().getDefaultShopSize();
	}

	@Override
	public int getPageCount() {
		//important to divide with largest inventory size so entries dont move to other pages when changing the row size
		return entryMap.isEmpty() ? 1 : entryMap.lastKey() / (9 * 6) + 1;
	}

	@Override
	public List<ShopEntry> getEntries(int shopPage) {
		int lowerBound = shopPage * (9 * 6);
		int upperBound = shopPage * (9 * 6) + rows * 9;

		return entryMap.entrySet().stream()
				.filter(e -> e.getKey() >= lowerBound && e.getKey() < upperBound)
				.map(Map.Entry::getValue)
				.collect(Collectors.toList());
	}

	@Override
	public void applyTemplate(EntryTemplate template) {
		throw new UnsupportedOperationException("Use applyTemplate(EntryTemplate t, int page) with page parameter to apply template to pagination");
	}

	@Override
	public void setPageRemembered(boolean rememberPage) {
		this.isPageRemembered = rememberPage;
	}

	public int getPreferredOpenPage(Customer customer) {
		return isPageRemembered ? customer.getPage(this, defaultPage) : defaultPage;
	}

	@Override
	public int getDefaultShopPage() {
		return defaultPage;
	}

	@Override
	public void setDefaultShopPage(int page) {
		this.defaultPage = page;
	}

	@Override
	public Component getPageTitle(int page) {
		if (!pageTitles.containsKey(page)) {
			return Component.empty();
		}
		return StatShops.getInstance().getMiniMessage().deserialize(pageTitles.get(page));
	}

	public String getPageTitleFormat(int page) {
		return pageTitles.getOrDefault(page, "");
	}

	@Override
	public void setPageTitle(int page, String titleFormat) {
		pageTitles.put(page, titleFormat);
	}

	public boolean open(Customer customer) {
		return open(customer, getPreferredOpenPage(customer));
	}

	@Override
	public boolean open(Customer customer, int page) {
		if (editor != null && !editor.getUniqueId().equals(customer.getUuid())) {
			customer.sendMessage(Message.SHOP_NOT_ENABLED);
			return false;
		}
		if (permission != null && !customer.getPlayer().hasPermission(permission)) {
			customer.sendMessage(Message.SHOP_NO_PERMISSION);
			return false;
		}
		if (!(page >= 0 && page < getPageCount())) {
			return false;
		}
		if (pageTurningPlayers.contains(customer.getUuid())) {
			pageTurningPlayers.remove(customer.getUuid());
			if (!handleShopTurnPage(customer, page)) {
				return false;
			}
		} else {
			if (!handleShopOpen(customer, page)) {
				return false;
			}
		}
		ChestShopMenu menu = new ChestShopMenu(this, customer);
		menu.setPage(customer.getPlayer(), page);
		menu.openSync(customer.getPlayer());

		menuMap.put(customer, menu);
		activeCustomers.add(customer);
		customer.setActiveShop(this);

		return true;
	}

	private boolean handleShopOpen(Customer customer, int page) {
		ShopOpenEvent shopOpenEvent = new ShopOpenEvent(this, customer, page);
		Bukkit.getPluginManager().callEvent(shopOpenEvent);
		return !shopOpenEvent.isCancelled();
	}

	private boolean handleShopClose(Customer customer) {
		ShopCloseEvent event = new ShopCloseEvent(this, 0); //TODo page
		Bukkit.getPluginManager().callEvent(event);

		if (!event.isCancelled()) {
			balanceMessenger.handleShopClose(customer.getPlayer());
			activeCustomers.remove(customer);
			return true;
		}
		return false;
	}

	private boolean handleShopTurnPage(Customer customer, int page) {
		ShopTurnPageEvent turnPageEvent = new ShopTurnPageEvent(this, customer, page);
		Bukkit.getPluginManager().callEvent(turnPageEvent);
		if (turnPageEvent.isCancelled()) {
			return false;
		}
		balanceMessenger.handlePageClose(customer.getPlayer());
		return true;
	}

	public void announceTurnPage(Customer customer) {
		pageTurningPlayers.add(customer.getUuid());
	}

	public boolean close(Customer customer) {
		ChestShopMenu menu = (ChestShopMenu) menuMap.get(customer);
		if (menu != null) {
			if (customer.getPlayer().getOpenInventory().getTopInventory().equals(menu.getInventory())) {
				customer.getPlayer().closeInventory();
			}
			menuMap.remove(customer);
		}
		customer.setActiveShop(null);
		return activeCustomers.remove(customer);
	}

	@Override
	public Menu newEditorMenu() {
		return new ChestShopEditor(this, getDefaultShopPage());
	}

	public EntryInteractionResult interact(Customer customer, int slot, EntryInteractionType interactionType) {
		if (editor != null && !customer.getUuid().equals(editor.getUniqueId())) {
			return EntryInteractionResult.FAIL_SHOP_DISABLED;
		}
		ShopEntry entry = getEntry(slot);
		if (entry == null) {
			return EntryInteractionResult.FAIL_NO_ENTRY;
		}
		ShopMenu menu = menuMap.get(customer);
		return entry.interact(customer, menu, interactionType);
	}

	public void setRows(int rows) {
		this.rows = rows % 6;
		if(this.rows <= 0) {
			this.rows += 6;
		}
		//all rows between 1 - 6
	}

	@Override
	public void applyTemplate(EntryTemplate template, int shopPage) {
		for (ShopEntry entry : template.getEntries(rows).values()) {
			int shopSlot = shopPage * (9 * 6) + entry.getSlot();
			ShopEntry newEntry = entry.duplicate();
			newEntry.setSlot(shopSlot);
			newEntry.setShop(this);
			addEntry(newEntry, shopSlot);
			newEntry.saveToDatabase();
		}
	}

	@Override
	public void applyDefaultTemplate(EntryTemplate template, int shopPage) {
		for (int page = 0; page <= shopPage; page++) {
			// Only override entries on actual page and not on any page before
			if (page != shopPage && getEntries(page).size() > 0) {
				continue;
			}
			for (ShopEntry entry : template.getEntries(rows).values()) {
				int shopSlot = page * (9 * 6) + entry.getSlot();
				if (getEntry(shopSlot) != null) {
					continue;
				}
				ShopEntry newEntry = entry.duplicate();
				newEntry.setSlot(shopSlot);
				newEntry.setShop(this);
				newEntry.saveToDatabase();
				addEntry(newEntry, shopSlot);
			}
		}
	}
}
