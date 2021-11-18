package de.bossascrew.shops.menu;

import com.google.common.base.Preconditions;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ClickContext;
import de.bossascrew.shops.menu.contexts.CloseContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.util.Pair;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Map;

public abstract class RowedOpenableMenu extends OpenableMenu {

	public static final int ROW_SIZE = 9;
	public static final int LARGEST_INV_SIZE = 6 * ROW_SIZE;

	@Getter
	private int rowCount = 3;

	public RowedOpenableMenu(Component title, InventoryType type, int rows, int backSlot, @Nullable Map<ClickType, ContextConsumer<ClickContext>> defaultClickHandler, @Nullable ContextConsumer<CloseContext> closeHandler) {
		super(title, type, new int[0], backSlot, defaultClickHandler, closeHandler);
		slots = getSlotsFromRows(rows);
		rowCount = rows;
	}

	public void setSpecialItem(int row, int column, @Nullable DefaultSpecialItem item) {
		setSpecialItem(calcIndex(row, column), item);
	}

	public void setItem(int row, int column, @Nullable ItemStack stack) {
		setItem(calcIndex(row, column), stack);
	}

	public void setItemAndClickHandler(int row, int column, DefaultSpecialItem item, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(calcIndex(row, column), item.createSpecialItem(), clickHandler);
	}

	public void setItemAndClickHandler(int row, int column, ItemStack item, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(calcIndex(row, column), item, clickHandler);
	}

	public void setItemAndClickHandler(int row, int column, @Nullable ItemStack stack, ClickType action, @Nullable ContextConsumer<ClickContext> clickHandler) {
		setItemAndClickHandler(calcIndex(row, column), stack, action, clickHandler);
	}

	public void setItemAndClickHandler(int row, int column, @Nullable ItemStack stack, @Nullable Map<ClickType, ContextConsumer<ClickContext>> clickHandler) {
		setItemAndClickHandler(calcIndex(row, column), stack, clickHandler);
	}

	public Pair<Integer, Integer> calcRowColumn(int index) {
		Preconditions.checkArgument(index >= 0, "index muss >= 0 sein");
		Preconditions.checkArgument(index < slots.length, "index muss < " + slots.length + " sein");

		int row = index / ROW_SIZE;
		int column = index % ROW_SIZE;

		return Pair.of(row, column);
	}

	public int calcIndex(int row, int column) {
		Preconditions.checkArgument(row >= 0, "row muss >= 0 sein");
		Preconditions.checkArgument(row < rowCount, "row muss < " + rowCount + " sein");
		Preconditions.checkArgument(column >= 0, "column muss >= 0 sein");
		Preconditions.checkArgument(column < ROW_SIZE, "column muss < " + ROW_SIZE + " sein");

		return row * ROW_SIZE + column;
	}

	private int[] getSlotsFromRows(int rows) {
		int size = rows * ROW_SIZE;
		int[] slots = new int[size];
		for (int i = 0; i < size; i++) {
			slots[i] = i;
		}
		return slots;
	}

	public void fillBottomLine() {
		fillBottomLine(backHandler);
	}

	public void fillBottomLine(ContextConsumer<BackContext> backHandler) {
		fillBottomLine(backHandler, DefaultSpecialItem.EMPTY_DARK);
	}

	public void fillBottomLine(@Nullable ContextConsumer<BackContext> backHandler, DefaultSpecialItem item) {
		for (int i = 0; i < 9; i++) {
			setItemAndClickHandler(rowCount - 1, i, DefaultSpecialItem.EMPTY_DARK, context -> {
			});
		}
		if (backHandler != null) {
			setItemAndClickHandler(backSlot, DefaultSpecialItem.BACK.createSpecialItem(), context -> backHandler.accept(new BackContext(context.getPlayer())));
		}
	}

	@Override
	public void fillMenu(@Nullable ContextConsumer<BackContext> backHandler, DefaultSpecialItem fillItem) {
		Preconditions.checkNotNull(fillItem, "fillItem");

		Arrays.stream(getSlots()).forEach(index -> setItemAndClickHandler(index, fillItem.createSpecialItem(), (ContextConsumer<ClickContext>) null));
		if (backHandler != null) {
			fillBottomLine(backHandler);
		}
	}
}
