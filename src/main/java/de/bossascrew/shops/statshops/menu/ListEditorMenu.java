package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.guiframework.inventory.*;
import de.cubbossa.guiframework.inventory.context.ClickContext;
import de.cubbossa.guiframework.inventory.context.ContextConsumer;
import de.cubbossa.guiframework.inventory.context.TargetContext;
import de.cubbossa.guiframework.inventory.implementations.AnvilMenu;
import de.cubbossa.guiframework.inventory.implementations.ListMenu;
import net.kyori.adventure.text.ComponentLike;
import org.bukkit.Material;
import org.bukkit.entity.Player;

import java.util.HashMap;
import java.util.Map;

public class ListEditorMenu<T> extends ListMenu {

    private final ListMenuSupplier<T> supplier;

    private final Map<Action<?>, ContextConsumer<TargetContext<T>>> clickHandler;

    public ListEditorMenu(ComponentLike title, int rows, ListMenuSupplier<T> supplier) {
        super(title, rows);
        this.supplier = supplier;
        this.clickHandler = new HashMap<>();

        addPreset(MenuPresets.fillRow(Icon.EMPTY_DARK_RP.create(), rows - 1));
        addPreset(MenuPresets.back(rows - 1, 8, Action.LEFT));
    }

    public void refreshElements() {
        clearListEntries();
        supplier.getElements().forEach(t -> {
            Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> map = new HashMap<>();
            for (Map.Entry<Action<?>, ContextConsumer<TargetContext<T>>> entry : clickHandler.entrySet()) {
                map.put(entry.getKey(), c -> {
                    var context = new TargetContext<>(c.getPlayer(), c.getMenu(), c.getSlot(), new Action<>(), c.isCancelled(), t);
                    entry.getValue().accept(context);
                    c.setCancelled(context.isCancelled());
                });
            }
            addListEntry(Button.builder().withItemStack(supplier.getDisplayItem(t)).withClickHandler(map));
        });
        refresh(getListSlots());
    }

    public void setInfoItem(Message name, Message lore) {
        addPreset(applier -> applier.addItem((getRows() - 1) * 9 + 4, ItemStackUtils.createItemStack(Material.PAPER, name, lore)));
    }

    public void setDeleteHandler(Message confirmTitle, Action<?> action, ContextConsumer<TargetContext<T>> clickHandler) {
        ContextConsumer<TargetContext<T>> extended = c -> {
            if (StatShops.getInstance().getShopsConfig().isConfirmDeletion()) {
                c.getMenu().openSubMenu(c.getPlayer(), () -> {
                    ConfirmMenu m = new ConfirmMenu(confirmTitle);
                    m.setDenyHandler(cl -> cl.getMenu().close(cl.getPlayer()));
                    m.setAcceptHandler(cl -> {
                        clickHandler.accept(c);
                        c.getMenu().close(c.getPlayer());
                        refreshElements();
                    });
                    return m;
                });
            } else {
                clickHandler.accept(c);
            }
        };
        this.clickHandler.put(action, extended);
        refreshElements();
    }

    public void setDuplicateHandler(Action<?> action, ContextConsumer<TargetContext<T>> clickHandler) {
        this.clickHandler.put(action, tTargetContext -> {
            clickHandler.accept(tTargetContext);
            refreshElements();
        });
    }

    public void setClickHandler(Action<?> action, ContextConsumer<TargetContext<T>> clickHandler) {
        this.clickHandler.put(action, tTargetContext -> {
            clickHandler.accept(tTargetContext);
            refreshElements();
        });
    }

    public void setNewHandler(Message name, Message lore, ContextConsumer<ClickContext> clickHandler) {
        addPreset(presetApplier -> {
            presetApplier.addItem((getRows() - 1) * 9 + 7, ItemStackUtils.createItemStack(Material.EMERALD, name, lore));
            presetApplier.addClickHandler((getRows() - 1) * 9 + 7, Action.LEFT, clickHandler);
        });
    }

    public void setNewHandlerStringInput(Message name, Message lore, Message title, String suggestion, ContextConsumer<TargetContext<String>> clickHandler) {
        addPreset(presetApplier -> {
            presetApplier.addItem((getRows() - 1) * 9 + 7, ItemStackUtils.createItemStack(Material.EMERALD, name, lore));
            presetApplier.addClickHandler((getRows() - 1) * 9 + 7, Action.LEFT, clickContext -> clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {
                AnvilMenu m = new AnvilMenu(title, suggestion);
                m.setOutputClickHandler(AnvilMenu.CONFIRM, c -> {
                    m.close(c.getPlayer());
                    clickHandler.accept(c);
                    refreshElements();
                });
                return m;
            }));
        });
    }

    @Override
    public void openSync(Player viewer, ViewMode viewMode) {
        refreshElements();
        super.openSync(viewer, viewMode);
    }
}
