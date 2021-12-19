package de.bossascrew.shops.general.menu;

import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ClickContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.statshops.data.Message;
import lombok.Data;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

public class PagedChestMenu {

	@Getter
	private final int rowCount;

	@Getter
	private final Component title;

	@Getter
	private final @Nullable
	Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler;

	@Getter
	private final @Nullable
	ContextConsumer<CloseContext> closeHandler;

	private final @Nullable
	ContextConsumer<BackContext> backHandler;

	private Comparator<ItemStack> sortingComparator = (i1, i2) -> 0;

	private final int entriesPerPage;

	@Getter
	protected int currentPage;

	protected final List<PagedMenuEntry> menuEntries = Lists.newArrayList();

	private final PagedMenuEntry[] navigationEntries = new PagedMenuEntry[RowedOpenableMenu.ROW_SIZE - 2];

	public PagedChestMenu(Message message, int rowCount) {
		this(message.getTranslation(), rowCount);
	}

	public PagedChestMenu(Component title, int rowCount) {
		this(title, rowCount, null, null, null);
	}

	public PagedChestMenu(Component title, int rowCount, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler,
						  @Nullable ContextConsumer<CloseContext> closeHandler, @Nullable ContextConsumer<BackContext> backHandler) {

		this.rowCount = rowCount;
		this.title = title;
		this.defaultClickHandler = defaultClickHandler;
		this.closeHandler = closeHandler;
		this.backHandler = backHandler;

		entriesPerPage = (rowCount - 1) * RowedOpenableMenu.ROW_SIZE;
	}

	public void addMenuEntry(ItemStack specialItem, @Nullable Map<ClickType, ContextConsumer<ClickContext>> clickHandler) {
		menuEntries.add(new PagedMenuEntry(specialItem, clickHandler));
	}

	public void addMenuEntry(ItemStack specialItem, ClickType type, ContextConsumer<ClickContext> clickHandler) {
		Map<ClickType, ContextConsumer<ClickContext>> c = new HashMap<>();
		c.put(type, clickHandler);
		menuEntries.add(new PagedMenuEntry(specialItem, c));
	}

	public void addMenuEntry(ItemStack specialItem, ContextConsumer<ClickContext> clickHandler) {
		Map<ClickType, ContextConsumer<ClickContext>> c = new HashMap<>();
		for (ClickType t : ClickType.values()) {
			c.put(t, clickHandler);
		}
		menuEntries.add(new PagedMenuEntry(specialItem, c));
	}

	public void clearMenuEntries() {
		menuEntries.clear();
	}

	public void setSortingComparator(@Nullable Comparator<ItemStack> sortingComparator) {
		if (sortingComparator == null) {
			this.sortingComparator = (i1, i2) -> 0;
		} else {
			this.sortingComparator = sortingComparator;
		}
	}

	public void setNavigationEntry(int column, ItemStack displayItemStack, @Nullable Map<ClickType, ContextConsumer<ClickContext>> clickHandler) {
		Preconditions.checkArgument(column >= 2 && column < RowedOpenableMenu.ROW_SIZE, "columns muss in [2," + RowedOpenableMenu.ROW_SIZE + "] liegen");
		navigationEntries[column - 2] = new PagedMenuEntry(displayItemStack, clickHandler);
	}

	public void setNavigationEntry(int column, ItemStack displayItemStack, @Nullable ContextConsumer<ClickContext> clickHandler) {
		Map<ClickType, ContextConsumer<ClickContext>> map = new HashMap<>();
		for (ClickType t : ClickType.values()) {
			map.put(t, clickHandler);
		}
		setNavigationEntry(column, displayItemStack, map);
	}

	public void setNavigationEntry(int column, ItemStack displayItemStack, ClickType action, @Nullable ContextConsumer<ClickContext> clickHandler) {
		Map<ClickType, ContextConsumer<ClickContext>> map = new HashMap<>();
		map.put(action, clickHandler);
		setNavigationEntry(column, displayItemStack, map);
	}

	public void setNavigationEntry(int column, ClickType action, @Nullable ContextConsumer<ClickContext> clickHandler) {
		PagedMenuEntry entry = navigationEntries[column - 2];
		if (entry != null && entry.getClickHandler() != null) {
			entry.getClickHandler().put(action, clickHandler);
		}
	}

	public ChestMenu getInvMenuForLastPage() {
		int page = menuEntries.size() / (rowCount - 1) * RowedOpenableMenu.ROW_SIZE;
		return getInvMenuForPage(page);
	}

	public ChestMenu getInvMenuForPage(int page) {
		ChestMenu menu = new ChestMenu(title, rowCount, defaultClickHandler, closeHandler);
		menu.fillMenu();
		menu.fillBottomLine(backHandler);
		menu.setItem(rowCount - 1, 5, DefaultSpecialItem.EMPTY_DARK_RP.createSpecialItem());

		AtomicInteger index = new AtomicInteger();
		menuEntries.stream().sorted(Comparator.comparing(PagedMenuEntry::getDisplayItemStack, sortingComparator)).skip((long) page * entriesPerPage)
				.limit(entriesPerPage).forEachOrdered(
						menuEntry -> menu.setItemAndClickHandler(index.getAndIncrement(), menuEntry.getDisplayItemStack(), menuEntry.getClickHandler()));

		AtomicInteger navigationIndex = new AtomicInteger(1);
		Arrays.stream(navigationEntries).peek(_x -> navigationIndex.getAndIncrement()).filter(Objects::nonNull)
				.forEachOrdered(menuEntry -> menu
						.setItemAndClickHandler(menu.getRowCount() - 1, navigationIndex.get(), menuEntry.getDisplayItemStack(),
								menuEntry.getClickHandler()));

		menu.setItemAndClickHandler(menu.getRowCount() - 1, 0, page > 0 ?
				DefaultSpecialItem.PREV_PAGE.createSpecialItem() : DefaultSpecialItem.PREV_PAGE_OFF.createSpecialItem(), c -> {
			if (page > 0) {
				openInventory(c.getPlayer(), page - 1);
			}
		});

		boolean hasPrev = (menuEntries.size() - 1) / entriesPerPage > page;
		menu.setItemAndClickHandler(menu.getRowCount() - 1, 1, hasPrev ?
				DefaultSpecialItem.NEXT_PAGE.createSpecialItem() : DefaultSpecialItem.NEXT_PAGE_OFF.createSpecialItem(), c -> {
			if (hasPrev) {
				openInventory(c.getPlayer(), page + 1);
			}
		});

		return menu;
	}

	public void openInventory(Player player) {
		openInventory(player, 0);
	}

	public void openInventory(Player player, int page) {
		this.currentPage = Integer.min(page, getPageCount() - 1);
		getInvMenuForPage(this.currentPage).openInventory(player);
	}

	public void openInventoryLastPage(Player player) {
		getInvMenuForLastPage().openInventory(player);
	}

	public int getPageCount() {
		return (this.menuEntries.size() - 1) / entriesPerPage + 1;
	}

	@Data
	private static class PagedMenuEntry {
		private final ItemStack displayItemStack;

		private final @Nullable
		Map<ClickType, ContextConsumer<ClickContext>> clickHandler;
	}
}
