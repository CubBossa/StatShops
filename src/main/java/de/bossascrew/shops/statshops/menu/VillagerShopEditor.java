package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.menuframework.inventory.Action;
import de.cubbossa.menuframework.inventory.ListMenuSupplier;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.Collection;

public class VillagerShopEditor extends ListEditorMenu<ShopEntry> {

    private final VillagerShop shop;
    @Getter
    private ShopEntry selectedEntry = null;

    public VillagerShopEditor(VillagerShop shop) {
        super(Message.VILLAGER_SHOP_TITLE, 3, null);
        super.setSupplier(new ListMenuSupplier<>() {
            @Override
            public Collection<ShopEntry> getElements() {
                return shop.getEntries().values();
            }

            @Override
            public ItemStack getDisplayItem(ShopEntry entry) {
                return getSelectedEntry() != null && getSelectedEntry().equals(entry) ?
                        ItemStackUtils.setGlow(entry.getDisplayItem()) :
                        entry.getDisplayItem();
            }
        });

        this.shop = shop;

        int left = 2 * 9 + 5;
        int right = 2 * 9 + 7;
        int edit = 2 * 9 + 6;

        ItemStack editStack = ItemStackUtils.createItemStack(Material.COMMAND_BLOCK, Message.GUI_VILLAGER_EDITOR_EDIT_NAME, Message.GUI_VILLAGER_EDITOR_EDIT_LORE);
        ItemStack leftStack = ItemStackUtils.setNameAndLore(Icon.STACK_PREV_PAGE_RP.clone(), Message.GUI_VILLAGER_EDITOR_LEFT_NAME, Message.GUI_VILLAGER_EDITOR_LEFT_LORE);
        ItemStack rightStack = ItemStackUtils.setNameAndLore(Icon.STACK_NEXT_PAGE_RP.clone(), Message.GUI_VILLAGER_EDITOR_RIGHT_NAME, Message.GUI_VILLAGER_EDITOR_RIGHT_LORE);

        addPreset(buttonHandler -> {
            // edit stack
            buttonHandler.addItem(edit, () -> getSelectedEntry() != null ? editStack : null);
            buttonHandler.addClickHandler(edit, Action.LEFT, c -> {
                if (getSelectedEntry() != null)
                    openSubMenu(c.getPlayer(), new ShopEntryEditor(getSelectedEntry(), c.getPlayer()));
            });

            // move up
            buttonHandler.addItem(left, () -> getSelectedEntry() != null ? leftStack : null);
            buttonHandler.addClickHandler(left, Action.LEFT, c -> {
                if (selectedEntry == null) {
                    return;
                }
                int i = selectedEntry.getSlot();
                if (i < 1) {
                    return;
                }
                // swap untip not null
                while (!shop.swapEntries(i, i - 1)) {
                }
            });

            // move down
            buttonHandler.addItem(right, () -> getSelectedEntry() != null ? rightStack : null);
            buttonHandler.addClickHandler(right, Action.LEFT, c -> {
                if (selectedEntry == null) {
                    return;
                }
                int i = selectedEntry.getSlot();
                if (i >= shop.getEntries().lastKey()) {
                    return;
                }
                // swap untip not null
                while (shop.swapEntries(i, i + 1)) {
                }
            });
        });
        setInfoItem(Message.GUI_VILLAGER_EDITOR_INFO_NAME, Message.GUI_VILLAGER_EDITOR_INFO_LORE);
        setDeleteHandler(Action.RIGHT, c -> {
            shop.deleteEntry(c.getTarget());
            if (selectedEntry.equals(c.getTarget())) {
                selectedEntry = null;
                refresh(left, right, edit);
            }
        });
        setNewHandler(Message.GUI_VILLAGER_EDITOR_NEW_NAME, Message.GUI_VILLAGER_EDITOR_NEW_LORE, c -> {
            Player player = c.getPlayer();
            if (player.getItemOnCursor() != null && !player.getItemOnCursor().getType().equals(Material.AIR)) {
                shop.createEntry(player.getItemOnCursor(), shop.getEntries().lastKey() + 1);
            }
        });
        setClickHandler(Action.LEFT, c -> {
            selectedEntry = selectedEntry.equals(c.getTarget()) ? null : c.getTarget();
            refresh(left, right, edit);
        });
    }
}
