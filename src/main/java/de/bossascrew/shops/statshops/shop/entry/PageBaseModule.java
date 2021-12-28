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

public class PageBaseModule extends BaseModule implements PageModule {

	private final Consumer3<Customer, ShopEntry, Integer> openPageHandler;

	private DataSlot.NumberSlot mode;
	private DataSlot.NumberSlot page;

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
		this.mode.setData(0);
		this.page.setData(page);
	}

	/**
	 * @param pageCount negative numbers to turn to pages before, positive to turn to next pages
	 */
	public void setNewPageRelative(int pageCount) throws IllegalArgumentException {
		if (pageCount == 0) {
			throw new IllegalArgumentException("The relative page cannot be 0.");
		}
		this.mode.setData(Math.abs(pageCount) / pageCount);
		System.out.println("mode: " + mode.getData().intValue());
		this.page.setData(Math.abs(pageCount));
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[] {page};
	}

	@Override
	public void loadData() {
		System.out.println("load data");
		page = shopEntry.getData(DataSlot.NumberSlot.class, "page", () -> {
			return new DataSlot.NumberSlot("page", 1., Message.GUI_ENTRY_FUNCTION_PAGE_NAME, Message.GUI_ENTRY_FUNCTION_PAGE_LORE);
		});
		mode = shopEntry.getData(DataSlot.NumberSlot.class, "mode", () -> {
			return new DataSlot.NumberSlot("mode", 1., Message.NONE, Message.NONE);
		});
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
		System.out.println(mode.getData());
		System.out.println(page.getData());
		int newPage = mode.getData().intValue() == 0 ?
				page.getData().intValue() - 1 :
				shopEntry.getSlot() / RowedOpenableMenu.LARGEST_INV_SIZE + page.getData().intValue() * mode.getData().intValue();
		System.out.println(newPage);
		this.openPageHandler.accept(customer, shopEntry, newPage);
	}

	@Override
	public EntryModule duplicate() {
		PageBaseModule module = new PageBaseModule(provider, shopEntry, openPageHandler);
		module.mode.setData(mode.getData());
		module.page.setData(page.getData());
		return module;
	}
}
