package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import de.bossascrew.shops.statshops.shop.entry.TradeBaseModule;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.menuframework.inventory.Action;
import de.cubbossa.menuframework.inventory.ListMenuSupplier;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.Collection;
import java.util.Objects;
import java.util.stream.Collectors;

public class VillagerShopEditor extends ListEditorMenu<ShopEntry> {

    private final VillagerShop shop;
    @Getter
    private ShopEntry selectedEntry = null;

    public VillagerShopEditor(VillagerShop shop) {
        super(Message.VILLAGER_SHOP_TITLE, 3, null);
        super.setSupplier(new ListMenuSupplier<>() {
            @Override
            public Collection<ShopEntry> getElements() {
                return shop.getEntries().values().stream().filter(Objects::nonNull).collect(Collectors.toSet());
            }

            @Override
            public ItemStack getDisplayItem(ShopEntry entry) {
                return selectedEntry != null && selectedEntry.equals(entry) ?
                        ItemStackUtils.setGlow(entry.getDisplayItem().clone()) :
                        entry.getDisplayItem().clone();
            }
        });

        this.shop = shop;

        final int edit = 2 * 9 + 2;
        final int left = 2 * 9 + 3;
        final int right = 2 * 9 + 4;

        ItemStack editStack = ItemStackUtils.createItemStack(Material.COMMAND_BLOCK, Message.GUI_VILLAGER_EDITOR_EDIT_NAME, Message.GUI_VILLAGER_EDITOR_EDIT_LORE);
        ItemStack leftStack = ItemStackUtils.setNameAndLore(Icon.STACK_PREV_PAGE_RP.clone(), Message.GUI_VILLAGER_EDITOR_LEFT_NAME, Message.GUI_VILLAGER_EDITOR_LEFT_LORE);
        ItemStack rightStack = ItemStackUtils.setNameAndLore(Icon.STACK_NEXT_PAGE_RP.clone(), Message.GUI_VILLAGER_EDITOR_RIGHT_NAME, Message.GUI_VILLAGER_EDITOR_RIGHT_LORE);

        addPreset(buttonHandler -> {
            // info
            buttonHandler.addItemOnTop(9 * 2 + 7, ItemStackUtils.createInfoItem(Message.GUI_VILLAGER_EDITOR_INFO_NAME, Message.GUI_VILLAGER_EDITOR_INFO_LORE));

            // edit stack
            buttonHandler.addItemOnTop(edit, () -> selectedEntry != null ? editStack.clone() : null);
            buttonHandler.addClickHandlerOnTop(edit, Action.LEFT, c -> {
                if (getSelectedEntry() != null)
                    openSubMenu(c.getPlayer(), MainMenu.newShopEntryEditor(getSelectedEntry(), c.getPlayer()));
            });

            // move up
            buttonHandler.addItemOnTop(left, () -> selectedEntry != null ? leftStack.clone() : null);
            buttonHandler.addClickHandlerOnTop(left, Action.LEFT, c -> {
                if (selectedEntry == null) {
                    return;
                }
                int i = selectedEntry.getSlot();
                if (i < 1) {
                    return;
                }
                shop.swapEntries(i, i - 1);
                refreshElements();
            });

            // move down
            buttonHandler.addItemOnTop(right, () -> selectedEntry != null ? rightStack.clone() : null);
            buttonHandler.addClickHandlerOnTop(right, Action.LEFT, c -> {
                if (selectedEntry == null) {
                    return;
                }
                int i = selectedEntry.getSlot();
                if (i >= shop.getEntries().lastKey()) {
                    return;
                }
                shop.swapEntries(i, i + 1);
                refreshElements();
            });
        });
        setDeleteHandler(Action.RIGHT, c -> {
            shop.deleteEntry(c.getTarget());
            if (selectedEntry != null && selectedEntry.equals(c.getTarget())) {
                selectedEntry = null;
                refresh(left, right, edit);
            }
        });
        setNewHandler(Message.GUI_VILLAGER_EDITOR_NEW_NAME, Message.GUI_VILLAGER_EDITOR_NEW_LORE, c -> {
            Player player = c.getPlayer();
            if (player.getItemOnCursor() != null && !player.getItemOnCursor().getType().equals(Material.AIR)) {
                ShopEntry entry = shop.createEntry(player.getItemOnCursor(), shop.getEntries().size() == 0 ? 0 : shop.getEntries().lastKey() + 1);
                //TODO entry.setModule();
                refreshElements();
            }
        });
        setClickHandler(Action.LEFT, c -> {
            selectedEntry = selectedEntry != null &&   selectedEntry.equals(c.getTarget()) ? null : c.getTarget();
            refreshElements();
            refresh(left, right, edit);
        });
    }
}
