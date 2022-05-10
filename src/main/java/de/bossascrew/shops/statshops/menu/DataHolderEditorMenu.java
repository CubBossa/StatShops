package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.api.DataSlotHolder;
import de.bossascrew.shops.statshops.shop.DataSlot;
import de.cubbossa.menuframework.inventory.Action;
import de.cubbossa.menuframework.inventory.Button;
import de.cubbossa.menuframework.inventory.MenuPresets;
import de.cubbossa.menuframework.inventory.implementations.ListMenu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.ComponentLike;
import org.bukkit.entity.Player;

import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.IntStream;

public class DataHolderEditorMenu extends ListMenu {

    private final DataSlotHolder dataHolder;
    @Getter
    @Setter
    private Function<DataSlotHolder, Iterable<DataSlot<?>>> dataProvider = x -> x.getData().values();

    public DataHolderEditorMenu(ComponentLike title, int startRow, DataSlotHolder dataHolder) {
        super(title, startRow + 3, IntStream.range((startRow + 1) * 9, (startRow + 3) * 9).toArray());
        this.dataHolder = dataHolder;
        addPreset(MenuPresets.fillRow(Icon.STACK_EMPTY_DARK, startRow));
        addPreset(presetApplier -> presetApplier.addItem(startRow * 9 + 5, Icon.STACK_EMPTY_DARK_RP));
        addPreset(MenuPresets.back(startRow, 8, Action.LEFT));
        addPreset(MenuPresets.paginationRow(startRow, 0, 1, true, Action.LEFT));
    }

    private void prepare() {
        clearContent();
        System.out.println("ListSlots: " + Arrays.toString(getListSlots()));
        for (DataSlot<?> ds : dataProvider.apply(dataHolder)) {
            addListEntry(Button.builder()
                    .withItemStack(ds::getDisplayItem)
                    .withClickHandler(ds.getClickHandler()));
        }
    }

    @Override
    public void openSync(Player viewer, ViewMode viewMode) {
        prepare();
        super.openSync(viewer, viewMode);
    }
}
