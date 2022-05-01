package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.Taggable;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.cubbossa.guiframework.inventory.*;
import de.cubbossa.guiframework.inventory.context.ContextConsumer;
import de.cubbossa.guiframework.inventory.context.TargetContext;
import de.cubbossa.guiframework.inventory.implementations.AnvilMenu;
import de.cubbossa.guiframework.inventory.implementations.ListMenu;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class StatShopMenus {

    public static <T> ListMenu newEditorMenu(ListMenuSupplier<T> supplier, ComponentLike title, int rows,
                                             Message newName, Message newLore,
                                             ComponentLike newTitle, String newPresetInput,
                                             Message infoName, Message infoLore,
                                             boolean confirmDeletion, ComponentLike confirmDeleteTitle,
                                             ContextConsumer<TargetContext<T>> leftClickHandler) {
        ListMenu menu = new ListMenu(title, rows);
        menu.addPreset(MenuPresets.fillRow(Icon.EMPTY_DARK_RP.create(), rows - 1));

        // info & new
        menu.addPreset(presetApplier -> {
            presetApplier.addItem((rows - 1) * 9 + 4, ItemStackUtils.createItemStack(Material.PAPER, infoName, infoLore));
            if (supplier instanceof ListMenuManagerSupplier<T> manager) {
                presetApplier.addItem((rows - 1) * 9 + 7, ItemStackUtils.createItemStack(Material.EMERALD, newName, newLore));
                presetApplier.addClickHandler((rows - 1) * 9 + 7, Action.LEFT, clickContext -> clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {
                    AnvilMenu m = new AnvilMenu(newTitle, newPresetInput);
                    m.setOutputClickHandler(AnvilMenu.CONFIRM, c -> {
                        manager.newElementFromMenu(new String[]{c.getTarget()});
                        m.close(c.getPlayer());
                    });
                    return m;
                }));
            }
        });
        // back
        menu.addPreset(MenuPresets.back(rows - 1, 8, Action.LEFT));

        Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> map = new HashMap<>();

        for (T element : supplier.getElements()) {
            map.put(Action.LEFT, t -> leftClickHandler.accept(new TargetContext<>(t.getPlayer(),
                    t.getMenu(), t.getSlot(), new Action<>(), t.isCancelled(), element)));
            if (supplier instanceof ListMenuManagerSupplier<T> manager) {
                map.put(Action.RIGHT, targetContext -> {
                    //  delete
                    if (confirmDeletion) {
                        targetContext.getMenu().openSubMenu(targetContext.getPlayer(), () -> {
                            ConfirmMenu m = new ConfirmMenu(confirmDeleteTitle);
                            m.setDenyHandler(c -> c.getPlayer().closeInventory());
                            m.setAcceptHandler(c -> {
                                manager.deleteFromMenu(element);
                                targetContext.getMenu().refresh(targetContext.getMenu().getSlots());
                                c.getPlayer().closeInventory();
                            });
                            return m;
                        });
                    } else {
                        manager.deleteFromMenu(element);
                        targetContext.getMenu().refresh(targetContext.getMenu().getSlots());
                    }
                });
                map.put(Action.MIDDLE, targetContext -> {
                    // duplicate
                    manager.duplicateElementFromMenu(element);
                    targetContext.getMenu().refresh(targetContext.getMenu().getSlots());
                });
            }
            menu.addListEntry(Button.builder()
                    .withItemStack(supplier.getDisplayItem(element))
                    .withClickHandler(map));
        }
        return menu;
    }

    public static <T extends Taggable> ListMenu newShopTaggableMenu(Shop shop, ListMenuSupplier<T> taggableSupplier, ComponentLike title, int rows,
                                               Message infoName, Message infoLore) {
        ListMenu menu = new ListMenu(title, rows);
        menu.addPreset(presetApplier -> {
            presetApplier.addItem(3 * 9 + 4, ItemStackUtils.createInfoItem(infoName, infoLore));
        });
        for (T taggable : taggableSupplier.getElements()) {
            menu.addListEntry(Button.builder()
                    .withItemStack(() -> {
                        ItemStack stack = taggableSupplier.getDisplayItem(taggable);
                        return TagUtils.hasCommonTags(shop, taggable) ? ItemStackUtils.setGlow(stack) : stack;
                    })
                    .withClickHandler(Action.LEFT, c -> {
                        taggable.addTag(shop.getUUID().toString());
                        c.getMenu().refresh(c.getSlot());
                    })
                    .withClickHandler(Action.RIGHT, c -> {
                        taggable.removeTag(shop.getUUID().toString());
                        c.getMenu().refresh(c.getSlot());
                    }));
        }
        return menu;
    }


    public static ListMenu newTagMenu(Taggable taggable, Component title,
                                      Message newTagTitle, Message newTagName, Message newTagLore, Message confirmRemove) {
        ListMenu menu = new ListMenu(title, 4);
        menu.addPreset(presetApplier -> {
            presetApplier.addItem(9 * 3 + 4, ItemStackUtils.createInfoItem(Message.GENERAL_GUI_TAGS_INFO_NAME, Message.GENERAL_GUI_TAGS_INFO_LORE));
            presetApplier.addItem(9 * 3 + 7, ItemStackUtils.createItemStack(Material.EMERALD, newTagName, newTagLore));
            presetApplier.addClickHandler(9 * 3 + 7, Action.LEFT, clickContext -> clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {
                AnvilMenu m = new AnvilMenu(newTagTitle, "tag-me");
                m.setOutputClickHandler(AnvilMenu.CONFIRM, c -> {
                    taggable.addTag(c.getTarget());
                    m.close(c.getPlayer());
                });
                return m;
            }));
        });
        menu.addListEntries(taggable.getTags(), s -> ItemStackUtils.createItemStack(Material.NAME_TAG, Component.text(s, NamedTextColor.WHITE), new ArrayList<>()), Action.LEFT, clickContext -> {
            String tag = clickContext.getTarget();
            clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {
                ConfirmMenu m = new ConfirmMenu(confirmRemove.getTranslation(TagResolver.resolver("name", Tag.inserting(Component.text(tag)))));
                m.setAcceptHandler(c -> {
                    taggable.removeTag(tag);
                    c.getPlayer().closeInventory();
                });
                m.setDenyHandler(c -> c.getPlayer().closeInventory());
                return m;
            });
        });
        return menu;
    }
}
