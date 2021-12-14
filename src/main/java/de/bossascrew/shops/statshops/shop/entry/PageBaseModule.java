package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.PageModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.util.Consumer3;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.function.Function;

public class PageBaseModule implements PageModule {

	@Getter
	private final ItemStack displayItem;
	@Getter
	private final Component displayName;
	@Getter
	private final List<Component> displayLore;
	@Getter
	@Setter
	private ShopEntry shopEntry;
	private final Consumer3<Customer, ShopEntry, Integer> openPageHandler;
	private Function<Integer, Integer> newPageProvider = null;
	private String data = "";

	public PageBaseModule(Message name, Message lore, ShopEntry shopEntry, Consumer3<Customer, ShopEntry, Integer> openPageHandler) {
		this(name.getTranslation(), lore.getTranslations(), shopEntry, openPageHandler);
	}

	public PageBaseModule(Component name, List<Component> lore, ShopEntry shopEntry, Consumer3<Customer, ShopEntry, Integer> openPageHandler) {
		this.shopEntry = shopEntry;
		this.openPageHandler = openPageHandler;
		this.displayName = name;
		this.displayLore = lore;
		displayItem = new ItemStack(Material.BOOK);
	}

	/**
	 * Open a static page
	 *
	 * @param page the page to open
	 */
	public void setNewPage(int page) {
		this.newPageProvider = integer -> page;
		this.data = "" + data;
	}

	/**
	 * @param pageCount negative numbers to turn to pages before, positive to turn to next pages
	 */
	public void setNewPageRelative(int pageCount) {
		this.newPageProvider = integer -> integer + pageCount;
		System.out.println(newPageProvider);
		this.data = pageCount < 0 ? "page" + pageCount : "page+" + pageCount;
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[0];
	}

	@Override
	public void loadData() {
		String function = shopEntry.getData(String.class, "page");

		if (function.startsWith("page+")) {
			this.newPageProvider = integer -> integer + Integer.parseInt(function.replace("page+", ""));
		} else if (function.startsWith("page-")) {
			this.newPageProvider = integer -> integer - Integer.parseInt(function.replace("page-", ""));
		} else {
			this.newPageProvider = integer -> Integer.parseInt(function);
		}
		this.data = function;
	}

	@Override
	public void saveData() {
		shopEntry.storeData(String.class, "page", data);
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, ShopInteractionResult result) {
		return null;
	}

	@Override
	public ShopInteractionResult perform(Customer customer, EntryInteractionType interactionType) {
		openPage(customer);
		return ShopInteractionResult.SUCCESS;
	}

	@Override
	public void openPage(Customer customer) {
		if (shopEntry.getShop() instanceof ChestMenuShop cms) {
			cms.announceTurnPage(customer); //TODO verallgmeeinern
		}
		this.openPageHandler.accept(customer, shopEntry, this.newPageProvider.apply(shopEntry.getSlot() / RowedOpenableMenu.LARGEST_INV_SIZE));
	}

	@Override
	public EntryModule duplicate() {
		PageBaseModule module = new PageBaseModule(displayName, displayLore, shopEntry, openPageHandler);
		module.newPageProvider = newPageProvider;
		return module;
	}
}
