package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.PaginatedShop;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.PageModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.util.Consumer3;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.Map;

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
	 * deserialize constructor. Provide shop entry afterwards!
	 */
	public PageBaseModule(Map<String, Object> values) {
		super(EntryModuleHandler.getInstance().getProvider((String) values.get("provider")), null);
		openPageHandler = (customer, entry, integer) -> {
			if (entry.getShop() instanceof PaginatedShop pShop) {
				pShop.open(customer, integer);
				return;
			}
			entry.getShop().open(customer);
		};
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
		this.page.setData(Math.abs(pageCount));
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[] {page};
	}

	@Override
	public void loadData() {
		page = shopEntry.getData(DataSlot.NumberSlot.class, "pagination_page", () -> {
			return new DataSlot.NumberSlot(1.);
		});
		mode = shopEntry.getData(DataSlot.NumberSlot.class, "pagination_mode", () -> {
			return new DataSlot.NumberSlot(1.);
		});
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		if (!StatShops.getInstance().getShopsConfig().isLogModulePagination()) {
			return null;
		}
		if (result != EntryInteractionResult.SUCCESS) {
			return null;
		}
		String type = switch (mode.getData().intValue()) {
			case -1 -> "previous page (" + page.getData() + " pages)";
			case 0 -> "exact page (" + page.getData() + ")";
			case 1 -> "next page (" + page.getData() + " pages)";
			default -> "unknown";
		};
		return new LogEntry("customer: '" + customer.getUuid().toString() +
				"', entry: '" + shopEntry.getUUID().toString() +
				"', type: '" + type +
				"', time: '" + LocalDateTime.now() + "'");
	}

	@Override
	public EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {
		openPage(customer);
		return EntryInteractionResult.SUCCESS;
	}

	@Override
	public void openPage(Customer customer) {
		if (shopEntry.getShop() instanceof ChestMenuShop cms) {
			cms.announceTurnPage(customer); //TODO verallgmeeinern
		}
		int newPage = mode.getData().intValue() == 0 ?
				page.getData().intValue() - 1 :
				shopEntry.getSlot() / RowedOpenableMenu.LARGEST_INV_SIZE + page.getData().intValue() * mode.getData().intValue();
		this.openPageHandler.accept(customer, shopEntry, newPage);
	}

	@Override
	public EntryModule duplicate() {
		PageBaseModule module = new PageBaseModule(provider, shopEntry, openPageHandler);
		module.mode.setData(mode.getData());
		module.page.setData(page.getData());
		return module;
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		return Map.of("provider", provider.getKey());
	}
}
