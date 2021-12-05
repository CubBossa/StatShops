package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.LogEntry;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.RowedOpenableMenu;
import de.bossascrew.shops.shop.ShopInteractionResult;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class PageBaseModule implements PageModule {

	@Getter
	private final ItemStack displayItem;
	@Getter
	private final Component displayName;
	@Getter
	private final List<Component> displayLore;
	private final ShopEntry shopEntry;
	private final BiConsumer<Customer, Integer> openPageHandler;
	private Function<Integer, Integer> newPageProvider = integer -> integer;
	private String data = "";

	public PageBaseModule(Message name, Message lore, ShopEntry shopEntry, BiConsumer<Customer, Integer> openPageHandler) {
		this.shopEntry = shopEntry;
		this.openPageHandler = openPageHandler;
		this.displayName = name.getTranslation();
		this.displayLore = lore.getTranslations();
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
		this.data = pageCount < 0 ? "page" + pageCount : "page+" + pageCount;
	}

	@Override
	public ItemStack getDisplayItem() {
		return new ItemStack(Material.BOOK);
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
	public @Nullable LogEntry createLogEntry() {
		return null;
	}

	@Override
	public ShopInteractionResult perform(Customer customer) {
		return ShopInteractionResult.SUCCESS;
	}

	@Override
	public void openPage(Customer customer) {
		this.openPageHandler.accept(customer, this.newPageProvider.apply(shopEntry.getSlot() % RowedOpenableMenu.LARGEST_INV_SIZE));
	}
}
