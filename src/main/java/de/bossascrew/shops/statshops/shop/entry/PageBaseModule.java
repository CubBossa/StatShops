package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.PageModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.util.Consumer3;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import org.jetbrains.annotations.Nullable;

import java.util.function.Function;

public class PageBaseModule extends BaseModule implements PageModule {

	private final Consumer3<Customer, ShopEntry, Integer> openPageHandler;
	private Function<Integer, Integer> newPageProvider = null;

	private DataSlot.IntegerSlot mode;
	private DataSlot.IntegerSlot page;

	public PageBaseModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry, Consumer3<Customer, ShopEntry, Integer> openPageHandler) {
		super(provider, shopEntry);
		this.openPageHandler = openPageHandler;
		loadData();
	}

	/**
	 * Open a static page
	 *
	 * @param page the page to open
	 */
	public void setNewPage(int page) {
		this.newPageProvider = integer -> page;
		this.page.setData(page);
		this.mode.setData(0);
	}

	/**
	 * @param pageCount negative numbers to turn to pages before, positive to turn to next pages
	 */
	public void setNewPageRelative(int pageCount) throws IllegalArgumentException {
		if (pageCount == 0) {
			throw new IllegalArgumentException("The relative page cannot be 0.");
		}
		this.newPageProvider = integer -> integer + pageCount;
		this.page.setData(Math.abs(pageCount));
		this.mode.setData(Math.abs(pageCount) / pageCount);
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[] {page};
	}

	@Override
	public void loadData() {
		page = shopEntry.getData(DataSlot.IntegerSlot.class, "page", () -> {
			return new DataSlot.IntegerSlot("page", 1, Message.GUI_ENTRY_FUNCTION_PAGE);
		});
		mode = shopEntry.getData(DataSlot.IntegerSlot.class, "mode", () -> {
			return new DataSlot.IntegerSlot("mode", 1, Message.NONE);
		});
		switch (mode.getData()) {
			case -1 -> this.newPageProvider = integer -> integer - page.getData();
			case 0 -> this.newPageProvider = integer -> page.getData();
			case 1 -> this.newPageProvider = integer -> integer + page.getData();
		}
	}

	@Override
	public void saveData() {
		shopEntry.storeData(page);
		shopEntry.storeData(mode);
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		return null;
	}

	@Override
	public EntryInteractionResult perform(Customer customer, EntryInteractionType interactionType) {
		openPage(customer);
		return EntryInteractionResult.SUCCESS;
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
		PageBaseModule module = new PageBaseModule(provider, shopEntry, openPageHandler);
		module.newPageProvider = newPageProvider;
		return module;
	}
}
