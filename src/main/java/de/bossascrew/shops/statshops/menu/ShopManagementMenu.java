package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.menu.*;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.TemplatableShop;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.*;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.bossascrew.shops.web.WebSessionUtils;
import de.bossascrew.shops.web.pasting.Paste;
import de.cubbossa.guiframework.inventory.Action;
import de.cubbossa.guiframework.inventory.ButtonBuilder;
import de.cubbossa.guiframework.inventory.Menu;
import de.cubbossa.guiframework.inventory.MenuPresets;
import de.cubbossa.guiframework.inventory.implementations.AnvilMenu;
import de.cubbossa.guiframework.inventory.implementations.InventoryMenu;
import de.cubbossa.guiframework.inventory.implementations.ListMenu;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public class ShopManagementMenu {

    public static void openBaseMenu(Player player) {

        InventoryMenu menu = new InventoryMenu(3, Message.GUI_MAIN_TITLE.getTranslation());
        menu.addPreset(MenuPresets.fill(MenuPresets.FILLER_LIGHT));

        // Main menu background texture
        ItemStack glass_rp = DefaultSpecialItem.EMPTY_LIGHT.createSpecialItem();
        ItemStackUtils.setCustomModelData(glass_rp, 7122001);
        menu.setItem(1, glass_rp);

        menu.setItemAndClickHandler(11, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
                Message.GUI_MAIN_LIMITS_NAME, Message.GUI_MAIN_LIMITS_LORE), Action.LEFT, clickContext ->
                clickContext.getMenu().openSubMenu(clickContext.getPlayer(), newLimitsMenu()));
        menu.setItemAndClickHandler(12, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
                Message.GUI_MAIN_DISCOUNTS_NAME, Message.GUI_MAIN_DISCOUNTS_LORE), Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), newDiscountsMenu()));
        menu.setItemAndClickHandler(13, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_SHOP,
                Message.GUI_MAIN_SHOPS_NAME, Message.GUI_MAIN_SHOPS_LORE), Action.LEFT, clickContext -> newShopsMenu());
        menu.setItem(14, ItemStackUtils.createItemStack(Material.WRITABLE_BOOK,
                Message.GUI_MAIN_LANGUAGE_NAME.getTranslation(), Message.GUI_MAIN_LANGUAGE_LORE.getTranslations(TagResolver.resolver("file",
                        Tag.inserting(Component.text(StatShops.getInstance().getShopsConfig().getLanguage() + ".yml"))))));
        menu.setClickHandler(14, Action.RIGHT, clickContext -> {
            long ms = System.currentTimeMillis();
            TranslationHandler.getInstance().loadLanguage(StatShops.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
                if (success) {
                    Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getKey(),
                            Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getTranslation(TagResolver.resolver("ms", Tag.inserting(Component.text(System.currentTimeMillis() - ms + "")))), 0);
                    openBaseMenu(player);
                    return;
                }
                Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOAD_ERROR);
                menu.setItem(14, DefaultSpecialItem.ERROR.createSpecialItem());
            });
        });
        menu.setClickHandler(14, Action.LEFT, clickContext -> {
            player.closeInventory();
            Customer customer = Customer.wrap(player);
            customer.sendMessage(Message.GENERAL_WEBINTERFACE_LOADING);

            StatShops.getInstance().runAsync(() -> {

                Paste paste = WebSessionUtils.generateWebSession();
                if (paste == null) {
                    customer.sendMessage(Message.GENERAL_WEBINTERFACE_ERROR);
                    return;
                }
                customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getKey(), Message.GENERAL_WEBINTERFACE_LINK.getTranslation(TagResolver.resolver("link", Tag.inserting(Component.text("https://127.0.0.1:8080/" + paste.getId())))));
            });
        });

        menu.setItemAndClickHandler(15, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_WEBINTERFACE,
                Message.GUI_MAIN_WEBINTERFACE_NAME, Message.GUI_MAIN_WEBINTERFACE_LORE), Action.LEFT, clickContext -> {
            player.closeInventory();
            Customer customer = Customer.wrap(player);
            customer.sendMessage(Message.GENERAL_WEBINTERFACE_LOADING);

            StatShops.getInstance().runAsync(() -> {

                Paste paste = WebSessionUtils.generateWebSession();
                if (paste == null) {
                    customer.sendMessage(Message.GENERAL_WEBINTERFACE_ERROR);
                    return;
                }
                customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getKey(), Message.GENERAL_WEBINTERFACE_LINK.getTranslation(TagResolver.resolver("link", Tag.inserting(Component.text("https://127.0.0.1:8080/" + paste.getId())))));
            });
        });
        menu.open(player);
    }

    public static Menu newShopsMenu() {
        int shops = ShopHandler.getInstance().getShops().size();
        return StatShopMenus.newEditorMenu(ShopHandler.getInstance(), Message.GUI_SHOPS_TITLE.getTranslation(), Integer.max(3, Integer.min(shops / 9, 6)),
                Message.GUI_SHOPS_NEW_NAME, Message.GUI_SHOPS_NEW_LORE,
                Message.GUI_SHOPS_NEW_TITLE.getTranslation(), "shop name",
                Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE,
                StatShops.getInstance().getShopsConfig().isConfirmDeletion(), Message.GUI_SHOPS_DELETE_CONFIRM.getTranslation(),
                c -> c.getMenu().openSubMenu(c.getPlayer(), newShopMenu(c.getTarget(), c.getPlayer())));
    }

    public static Menu newShopMenu(Shop shop, Player viewer) {

        InventoryMenu menu = new InventoryMenu(3, shop.getName());
        menu.addPreset(MenuPresets.fill(MenuPresets.FILLER_LIGHT));


        //Set name
        menu.setItemAndClickHandler(1, ItemStackUtils.createItemStack(shop.getDisplayItem() == null ?
                        new ItemStack(ItemStackUtils.MATERIAL_SHOP) : shop.getDisplayItem(), Message.GUI_SHOP_SET_NAME_NAME, Message.GUI_SHOP_SET_NAME_LORE),
                Action.LEFT, c -> {
                    if (c.getPlayer().getItemOnCursor().getType() != Material.AIR) {
                        shop.setDisplayItem(c.getPlayer().getItemOnCursor().clone());
                        StatShops.getInstance().getDatabase().saveShop(shop);
                        c.getMenu().setItem(c.getSlot(), ItemStackUtils.createItemStack(shop.getDisplayItem(), Message.GUI_SHOP_SET_NAME_NAME, Message.GUI_SHOP_SET_NAME_LORE));
                        c.getMenu().refresh(c.getSlot());
                        return;
                    }
                    c.getMenu().openSubMenu(c.getPlayer(), () -> {
                        AnvilMenu m = new AnvilMenu(Message.GUI_SHOP_SET_NAME_TITLE.getTranslation(), shop.getNameFormat());
                        m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                            shop.setNameFormat(s.getTarget());
                            s.getPlayer().closeInventory();
                            menu.updateTitle(shop.getName());
                        });
                        return m;
                    });
                });

        //Set permissions
        menu.setItemAndClickHandler(2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
                        Message.GUI_SHOP_SET_PERMISSION_NAME.getTranslation(), Message.GUI_SHOP_SET_PERMISSION_LORE.getTranslations(
                                TagResolver.resolver("permission", Tag.inserting(Component.text(shop.getPermission() == null ? "X" : shop.getPermission()))))),
                Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
                    AnvilMenu m = new AnvilMenu(Message.GUI_SHOP_SET_PERMISSION_TITLE.getTranslation(), "shops.shop." + shop.getNamePlain().toLowerCase() + ".");
                    m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                        shop.setPermission(s.getTarget());
                        c.getMenu().refresh(c.getSlot());
                        s.getPlayer().closeInventory();
                    });
                    return m;
                }));

        //Open Tags menu
        menu.setItemAndClickHandler(4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
                        Message.GUI_SHOP_SET_TAGS_NAME, Message.GUI_SHOP_SET_TAGS_LORE),
                Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), StatShopMenus.newTagMenu(shop,
                        Message.GUI_TAGS_TITLE.getTranslation(TagResolver.resolver("name", Tag.inserting(shop.getName()))),
                        Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
                        Message.GENERAL_GUI_TAGS_REMOVE_TAG)));

        //Open Limits menu
        List<Component> limitsLore = new ArrayList<>();
        Pair<Limit, Limit> limits = LimitsHandler.getInstance().getMinimalLimitsWithMatchingTags(null, shop);
        ItemStackUtils.addLoreLimits(limitsLore, limits.getLeft(), limits.getRight(), 0);
        if (limitsLore.size() > 0) {
            limitsLore.add(Message.SHOP_ITEM_LORE_SPACER.getTranslation());
        }
        limitsLore.addAll(Message.GUI_SHOP_SET_LIMITS_LORE.getTranslations());
        menu.setItemAndClickHandler(5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Message.GUI_SHOP_SET_LIMITS_NAME.getTranslation(), limitsLore),
                Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), newShopLimitsMenu(shop)));
        ;

        //Open Discounts menu
        List<Component> discountLore = new ArrayList<>();
        List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(viewer, shop);
        ItemStackUtils.addLoreDiscount(discountLore, discounts);
        if (discountLore.size() > 0) {
            discountLore.add(Message.SHOP_ITEM_LORE_SPACER.getTranslation());
        }
        discountLore.addAll(Message.GUI_SHOP_SET_DISCOUNTS_LORE.getTranslations());
        menu.setItemAndClickHandler(6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_SHOP_SET_DISCOUNTS_NAME.getTranslation(), discountLore),
                Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), openShopDiscountsMenu(shop)));

        if (shop instanceof TemplatableShop tShop) {
            //Open Templates menu
            menu.setItemAndClickHandler(7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
                            Message.GUI_SHOP_SET_TEMPLATE_NAME.getTranslation(), Message.GUI_SHOP_SET_TEMPLATE_LORE.getTranslations(TagResolver.resolver("template",
                                    Tag.inserting(tShop.getDefaultTemplate() == null ? Component.text("none") : tShop.getDefaultTemplate().getName())))),
                    Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), openDefaultTemplateMenu(tShop)));
        }

        //Assign Shop to NPC
        if (StatShops.getInstance().isCitizensInstalled()) {

            List<String> assigned = StatShops.getInstance().getCitizensHook().getAssignedNPCs(shop);
            menu.setItemAndClickHandler(8, ItemStackUtils.createItemStack(Material.PLAYER_HEAD,
                            Message.GUI_SHOP_SET_NPC_NAME.getTranslation(), Message.GUI_SHOP_SET_NPC_LORE.getTranslations(
                                    TagResolver.resolver("current", Tag.inserting(Component.text(assigned.isEmpty() ?
                                            "none" : String.join("<gray>, </gray>", assigned)))))),
                    Action.LEFT, c -> {
                        StatShops.getInstance().getCitizensHook().addAssigningPlayer(c.getPlayer(), shop);
                        c.getPlayer().closeInventory();
                        Customer.wrap(c.getPlayer()).sendMessage(Message.CITIZENS_CLICK_TO_ASSIGN);
                    });
        }

        menu.setItemAndClickHandler(9 + 1, ItemStackUtils.createItemStack(Material.SMITHING_TABLE,
                        Message.GUI_SHOP_SET_CONTENT_NAME, Message.GUI_SHOP_SET_CONTENT_LORE),
                Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), shop.newEditorMenu()));

        //TODO bottom inventory
        menu.setItemAndClickHandler(9 + 2, ItemStackUtils.createItemStack(Material.CHEST,
                Message.GUI_SHOP_SET_PREVIEW_NAME, Message.GUI_SHOP_SET_PREVIEW_LORE), Action.LEFT, c -> shop.open(Customer.wrap(c.getPlayer())));


        if (shop instanceof PaginatedShop ps) {

            // default page
            menu.setButton(22, ButtonBuilder.buttonBuilder()
                    .withItemStack(getDefaultPageItem(ps, ps.getDefaultShopPage()))
                    .withClickHandler(Action.LEFT, c -> {
                        ps.setDefaultShopPage((ps.getDefaultShopPage() - 1) % ps.getPageCount());
                        c.getMenu().setItem(22, getDefaultPageItem(ps, ps.getDefaultShopPage()));
                        c.getMenu().refresh(22);
                    })
                    .withClickHandler(Action.RIGHT, c -> {
                        ps.setDefaultShopPage((ps.getDefaultShopPage() + 1) % ps.getPageCount());
                        c.getMenu().setItem(22, getDefaultPageItem(ps, ps.getDefaultShopPage()));
                        c.getMenu().refresh(22);
                    }));

            // page remembered
            Supplier<ItemStack> rememberPage = () -> ItemStackUtils.createButtonItemStack(ps.isPageRemembered(),
                    Message.GUI_SHOP_SET_REMEMBER_PAGE_NAME, Message.GUI_SHOP_SET_REMEMBER_PAGE_LORE);

            menu.setButton(24, ButtonBuilder.buttonBuilder()
                    .withItemStack(rememberPage.get())
                    .withClickHandler(Action.LEFT, c -> {
                        ps.setPageRemembered(!ps.isPageRemembered());
                        c.getMenu().setItem(24, rememberPage.get());
                        c.getMenu().refresh(c.getSlot());
                    }));
        }

        if (shop instanceof ChestMenuShop chestMenuShop) {

            // rows
            menu.setButton(23, ButtonBuilder.buttonBuilder()
                    .withItemStack(getRowsItem(chestMenuShop.getRows()))
                    .withClickHandler(Action.LEFT, c -> {
                        chestMenuShop.setRows(chestMenuShop.getRows() + 1);
                        c.getMenu().setItem(c.getSlot(), getRowsItem(chestMenuShop.getRows()));
                        c.getMenu().refresh(c.getSlot());
                    })
                    .withClickHandler(Action.RIGHT, c -> {
                        chestMenuShop.setRows(chestMenuShop.getRows() - 1);
                        c.getMenu().setItem(c.getSlot(), getRowsItem(chestMenuShop.getRows()));
                        c.getMenu().refresh(c.getSlot());
                    }));
        }

        menu.setCloseHandler(closeContext -> shop.saveToDatabase());
        return menu;
    }

    private static ItemStack getRowsItem(int row) {
        List<Component> lore = new ArrayList<>();
        lore.add(Message.GUI_SHOP_SET_ROWS_LORE.getTranslation(TagResolver.resolver("rows", Tag.inserting(Component.text("" + row)))));

        return ItemStackUtils.createItemStack(new ItemStack(Material.RAIL, row), Message.GUI_SHOP_SET_ROWS_NAME.getTranslation(
                TagResolver.resolver("rows", Tag.inserting(Component.text("" + row)))), lore);
    }

    private static ItemStack getDefaultPageItem(PaginatedShop shop, int page) {
        int pageCount = shop.getPageCount();
        List<Component> lore = new ArrayList<>();
        lore.add(Message.GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(TagResolver.resolver("page", Tag.inserting(Component.text("" + (page + 1)))), TagResolver.resolver("pages", Tag.inserting(Component.text("" + pageCount)))));
        return ItemStackUtils.createItemStack(new ItemStack(Material.BOOK, Integer.min(Integer.max(page + 1, 1), 127)),
                Message.GUI_SHOP_SET_DEFAULT_PAGE_NAME.getTranslation(TagResolver.resolver("page", Tag.inserting(Component.text("" + (page + 1)))), TagResolver.resolver("pages", Tag.inserting(Component.text("" + pageCount)))), lore);
    }

    public static Menu newShopLimitsMenu(Shop shop) {
        ListMenu menu = new ListMenu(3, Message.GUI_SHOP_LIMITS_TITLE.getTranslation());
        menu.addPreset(presetApplier -> {
            presetApplier.addItem(4, ItemStackUtils.createInfoItem(Message.GUI_SHOP_LIMITS_INFO_NAME, Message.GUI_SHOP_LIMITS_INFO_LORE));
        });
        menu.addListEntries(LimitsHandler.getInstance().getElements(),
                l -> {
                    ItemStack i = LimitsHandler.getInstance().getDisplayItem(l);
                    if (TagUtils.hasCommonTags(shop, l)) {
                        return ItemStackUtils.setGlow(i);
                    }
                    return i;
                },
                Action.LEFT,
                targetContext -> {
                    Limit limit = targetContext.getTarget();
                    if (targetContext.getAction() == Action.RIGHT) {
                        limit.removeTag(shop.getUUID().toString());
                    } else if (targetContext.getAction() == Action.LEFT) {
                        limit.addTag(shop.getUUID().toString());
                    }
                    targetContext.getMenu().refresh(targetContext.getMenu().getSlots());
                });
        return menu;
    }

    public static Menu openShopDiscountsMenu(Shop shop) {
        LMenu<Discount> listMenu = new LMenu<>(3, DiscountHandler.getInstance(), Message.GUI_SHOP_DISCOUNTS_TITLE, backContext -> newShopMenu(shop));
        listMenu.setNavigationEntry(4, ItemStackUtils.createInfoItem(Message.GUI_SHOP_DISCOUNTS_INFO_NAME, Message.GUI_SHOP_DISCOUNTS_INFO_LORE), clickContext -> {
        });
        listMenu.setGlowPredicate(discount -> TagUtils.hasCommonTags(shop, discount));
        listMenu.setClickHandler(targetContext -> {
            Discount discount = targetContext.getTarget();
            if (targetContext.getAction().isRightClick()) {
                discount.removeTag(shop.getUUID().toString());
            } else if (targetContext.getAction().isLeftClick()) {
                discount.addTag(shop.getUUID().toString());
            }
            listMenu.openInventory(player, listMenu.getCurrentPage());
        });
        listMenu.openInventory(player, page);
    }

    public static Menu openDefaultTemplateMenu(TemplatableShop shop) {
        LMenu<EntryTemplate> listMenu = new LMenu<>(3, TemplateHandler.getInstance(), Message.GUI_SHOP_TEMPLATE_TITLE, backContext -> newShopMenu(player, shop, fromPage));
        listMenu.setNavigationEntry(4, ItemStackUtils.createInfoItem(Message.GUI_SHOP_TEMPLATE_INFO_NAME, Message.GUI_SHOP_TEMPLATE_INFO_LORE), clickContext -> {
        });
        listMenu.setGlowPredicate(template -> shop.getDefaultTemplate() != null && template.equals(shop.getDefaultTemplate()));
        listMenu.setClickHandler(targetContext -> {
            EntryTemplate template = targetContext.getTarget();
            if (shop.getDefaultTemplate() != null && template.equals(shop.getDefaultTemplate())) {
                shop.setDefaultTemplate(null);
            } else {
                shop.setDefaultTemplate(template);
            }
            openDefaultTemplateMenu(shop);
        });
        listMenu.openInventory(player, page);
    }

    public static ListMenu newLimitsMenu() {
        return StatShopMenus.newEditorMenu(LimitsHandler.getInstance(), Message.GUI_LIMITS.getTranslation(), 3,
                Message.GUI_LIMITS_NEW_NAME, Message.GUI_LIMITS_NEW_LORE, Message.GUI_LIMITS_NEW_TITLE.getTranslation(), "new limit",
                Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE, StatShops.getInstance().getShopsConfig().isConfirmDeletion(),
                Message.GUI_LIMITS_DELETE_CONFIRM.getTranslation(), c -> c.getMenu().openSubMenu(c.getPlayer(), newLimitMenu(c.getTarget())));
    }

    public static Menu newLimitMenu(Limit limit) {

        InventoryMenu menu = new InventoryMenu(3, limit.getName());
        menu.setCloseHandler(closeContext -> limit.saveToDatabase());
        //Set name
        menu.setItemAndClickHandler(1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
                Message.GUI_LIMIT_SET_NAME_NAME, Message.GUI_LIMIT_SET_NAME_LORE), Action.LEFT, c -> {
            AnvilMenu m = new AnvilMenu(Message.GUI_LIMIT_SET_NAME_TITLE.getTranslation(), limit.getNameFormat());
            m.setOutputClickHandler(AnvilMenu.CONFIRM, clickContext -> limit.setNameFormat(clickContext.getTarget()));
            menu.openSubMenu(c.getPlayer(), m);
        });
        //Set permissions
        menu.setItemAndClickHandler(2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
                Message.GUI_LIMIT_SET_PERMISSION_NAME.getTranslation(),
                Message.GUI_LIMIT_SET_PERMISSION_LORE.getTranslations(TagResolver.resolver("permission",
                        Tag.inserting(Component.text(limit.getPermission() == null ? "X" : limit.getPermission())))
                )), Action.LEFT, c -> {

            AnvilMenu m = new AnvilMenu(Message.GUI_LIMIT_SET_PERMISSION_TITLE.getTranslation(), "shops.limit." + limit.getNamePlain().toLowerCase() + ".");
            m.setOutputClickHandler(AnvilMenu.CONFIRM, clickContext -> limit.setPermission(clickContext.getTarget()));
            menu.openSubMenu(c.getPlayer(), m);
        });
        //Open Tags menu
        menu.setItemAndClickHandler(4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
                        Message.GUI_LIMIT_SET_TAGS_NAME, Message.GUI_LIMIT_SET_TAGS_LORE), Action.LEFT,
                clickContext -> clickContext.getMenu().openSubMenu(clickContext.getPlayer(),
                        StatShopMenus.newTagMenu(limit, Message.GUI_TAGS_TITLE.getTranslation(TagResolver.resolver("name", Tag.inserting(limit.getName()))),
                                Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE, Message.GENERAL_GUI_TAGS_REMOVE_TAG)));

        Supplier<ItemStack> durationStack = () -> ItemStackUtils.createItemStack(Material.COMPASS, Message.GUI_LIMIT_SET_DURATION_NAME.getTranslation(),
                Message.GUI_LIMIT_SET_DURATION_LORE.getTranslations(TagResolver.resolver("current", Tag.inserting(Component.text(TextUtils.formatDuration(limit.getRecover()))))));
        menu.setItemAndClickHandler(5, durationStack.get(), Action.LEFT, clickContext -> {
            clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {
                AnvilMenu m = new AnvilMenu(Message.GUI_LIMIT_SET_DURATION_TITLE.getTranslation(), TextUtils.DURATION_FORMAT);
                m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                    Player player = s.getPlayer();
                    Duration duration = TextUtils.parseDuration(s.getTarget());
                    if (duration == null) {
                        player.playSound(player.getLocation(), Sound.ENTITY_VILLAGER_NO, .5f, 1f);
                        return;
                    }
                    limit.setRecover(duration);
                    menu.setItem(5, durationStack.get());
                    menu.refresh(5);
                    player.closeInventory();
                });
                return m;
            });
        });

        Supplier<ItemStack> limitStack = () -> ItemStackUtils.createItemStack(Material.PAPER, Message.GUI_LIMIT_SET_LIMIT_NAME.getTranslation(),
                Message.GUI_LIMIT_SET_LIMIT_LORE.getTranslations(TagResolver.resolver("current", Tag.inserting(Component.text(limit.getTransactionLimit() + "")))));

        menu.setItemAndClickHandler(6, limitStack.get(), Action.LEFT, clickContext -> {
            clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {
                AnvilMenu m = new AnvilMenu(Message.GUI_LIMIT_SET_LIMIT_TITLE.getTranslation(), "64 ");
                m.setOutputClickHandler(AnvilMenu.CONFIRM, c -> {
                    Player player = c.getPlayer();
                    try {
                        limit.setTransactionLimit(Integer.parseInt(c.getTarget()));
                        menu.setItem(6, limitStack.get());
                        menu.refresh(6);
                        player.closeInventory();
                    } catch (NumberFormatException ignored) {
                        player.playSound(player.getLocation(), Sound.ENTITY_VILLAGER_NO, .5f, 1f);
                    }

                });
                return m;
            });
        });

        Supplier<ItemStack> globalStack = () -> ItemStackUtils.createButtonItemStack(limit.isGlobal(), Message.GUI_LIMIT_SET_GLOBAL_NAME,
                Message.GUI_LIMIT_SET_GLOBAL_LORE);

        menu.setItemAndClickHandler(7, globalStack.get(), Action.LEFT, clickContext -> {
            limit.setGlobal(!limit.isGlobal());
            menu.setItem(7, globalStack.get());
            menu.refresh(7);
        });

        return menu;
    }

    public static Menu newDiscountsMenu() {
        return StatShopMenus.newEditorMenu(DiscountHandler.getInstance(), Message.GUI_DISCOUNTS.getTranslation(), 3,
                Message.GUI_DISCOUNTS_NEW_NAME, Message.GUI_DISCOUNTS_NEW_LORE, Message.GUI_DISCOUNTS_NEW_TITLE.getTranslation(), "",
                Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE,
                StatShops.getInstance().getShopsConfig().isConfirmDeletion(), Message.GUI_DISCOUNTS_DELETE_CONFIRM.getTranslation(), c -> {
                    c.getMenu().openSubMenu(c.getPlayer(), newDiscountMenu(c.getTarget()));
                });
    }

    public static Menu newDiscountMenu(Discount discount) {
        ChestMenu chestMenu = new ChestMenu(Message.GUI_DISCOUNT, 3);
        chestMenu.setBackHandlerAction(backContext -> newDiscountsMenu(player, fromPage));
        chestMenu.setCloseHandler(closeContext -> {
            discount.setEditor(null);
            discount.saveToDatabase();
        });

        //Set name
        chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_DISCOUNT_SET_NAME_NAME, Message.GUI_DISCOUNT_SET_NAME_LORE), clickContext -> {
            player.closeInventory();
            new AnvilGUI.Builder()
                    .plugin(StatShops.getInstance())
                    .text(discount.getNameFormat())
                    .title(Message.GUI_DISCOUNT_SET_NAME_TITLE.getLegacyTranslation())
                    .onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> newDiscountMenu(p, discount, fromPage), 1L))
                    .onComplete((p, s) -> {
                        discount.setNameFormat(s);
                        newDiscountMenu(player, discount, fromPage);
                        return AnvilGUI.Response.close();
                    }).open(player);
        });
        //Set permissions
        chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
                Message.GUI_DISCOUNT_SET_PERMISSION_NAME.getTranslation(), Message.GUI_DISCOUNT_SET_PERMISSION_LORE.getTranslations(
                        TagResolver.resolver("permission", Tag.inserting(Component.text(discount.getPermission() == null ? "X" : discount.getPermission())))
                )), clickContext -> {
            player.closeInventory();
            new AnvilGUI.Builder()
                    .plugin(StatShops.getInstance())
                    .text("shops.discount." + discount.getNamePlain().toLowerCase() + ".")
                    .title(Message.GUI_DISCOUNT_SET_PERMISSION_TITLE.getLegacyTranslation())
                    .onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> newDiscountMenu(p, discount, fromPage), 1L))
                    .onComplete((p, s) -> {
                        discount.setPermission(s);
                        newDiscountMenu(player, discount, fromPage);
                        return AnvilGUI.Response.close();
                    }).open(player);
        });
        //Open Tags menu
        chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
                        Message.GUI_DISCOUNT_SET_TAGS_NAME, Message.GUI_DISCOUNT_SET_TAGS_LORE),
                clickContext -> new TagsEditorMenu<>(
                        discount, Message.GUI_TAGS_TITLE.getTranslation(TagResolver.resolver("name", Tag.inserting(discount.getName()))),
                        Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
                        Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> newDiscountMenu(player, discount, fromPage)).openInventory(player));

        chestMenu.setItemAndClickHandler(0, 5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DATES, Message.GUI_DISCOUNT_SET_START_NAME,
                Message.GUI_DISCOUNT_SET_START_LORE), clickContext -> openDiscountStartMenu(clickContext.getPlayer(), discount, fromPage, 0));

        TagResolver dur = TagResolver.resolver("duration", Tag.inserting(Component.text(TextUtils.formatDuration(discount.getDuration()))));
        chestMenu.setItemAndClickHandler(0, 6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DURATIONS, Message.GUI_DISCOUNT_SET_DURATION_NAME.getTranslation(dur),
                Message.GUI_DISCOUNT_SET_DURATION_LORE.getTranslations(dur)), clickContext -> {

            player.closeInventory();
            new AnvilGUI.Builder()
                    .plugin(StatShops.getInstance())
                    .text(TextUtils.DURATION_FORMAT)
                    .title(Message.GUI_DISCOUNT_SET_DURATION_TITLE.getLegacyTranslation())
                    .onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> {
                        chestMenu.refresh(6);
                        chestMenu.openInventory(player);
                    }, 1L))
                    .onComplete((p, s) -> {
                        discount.setDuration(TextUtils.parseDuration(s));
                        return AnvilGUI.Response.close();
                    }).open(player);
        });

        TagResolver percent = TagResolver.resolver("percent", Tag.inserting(discount.getFormattedPercent()));
        chestMenu.setItemAndClickHandler(0, 7, ItemStackUtils.createItemStack(Material.EMERALD, Message.GUI_DISCOUNT_SET_PERCENT_NAME.getTranslation(percent),
                Message.GUI_DISCOUNT_SET_PERCENT_LORE.getTranslations(percent)), clickContext -> {

            player.closeInventory();
            new AnvilGUI.Builder()
                    .plugin(StatShops.getInstance())
                    .text("50%")
                    .title(Message.GUI_DISCOUNT_SET_PERCENT_TITLE.getLegacyTranslation())
                    .onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> {
                        chestMenu.refresh(7);
                        chestMenu.openInventory(player);
                    }, 1L))
                    .onComplete((p, s) -> {
                        double d = 0;
                        try {
                            d = Double.parseDouble(s.replace("%", ""));
                        } catch (NumberFormatException ignored) {
                        }
                        discount.setPercent(d);
                        chestMenu.refresh(7);
                        chestMenu.openInventory(player);
                        return AnvilGUI.Response.close();
                    }).open(player);
        });

        chestMenu.openInventory(player);
    }

    public void openDiscountStartMenu(Player player, Discount discount, int fromPage, int page) {
        PagedChestMenu menu = new PagedChestMenu(Message.GUI_DISCOUNT_SET_START_TITLE.getTranslation(), 3, null, null, backContext -> {
            newDiscountMenu(player, discount, fromPage);
        });

        menu.setNavigationEntry(4, ItemStackUtils.createInfoItem(Message.GUI_DISCOUNT_START_INFO_NAME, Message.GUI_DISCOUNT_START_INFO_LORE), cc -> {
        });

        menu.setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD, Message.GUI_DISCOUNT_START_NEW_NAME,
                Message.GUI_DISCOUNT_START_NEW_LORE), cc -> {

            player.closeInventory();
            new AnvilGUI.Builder()
                    .plugin(StatShops.getInstance())
                    .text(TextUtils.DATE_TIME_FORMAT)
                    .title(Message.GUI_DISCOUNT_START_NEW_TITLE.getLegacyTranslation())
                    .onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openDiscountStartMenu(player, discount, fromPage, menu.getCurrentPage()), 1L))
                    .onComplete((p, s) -> {
                        LocalDateTime date = TextUtils.parseLocalDateTime(s);
                        if (date != null) {
                            discount.addStartTime(date);
                        }
                        return AnvilGUI.Response.close();
                    }).open(player);
        });
        for (LocalDateTime date : discount.getStartTimes()) {
            Component dateComp = Component.text(TextUtils.formatLocalDateTime(date), NamedTextColor.WHITE);
            menu.addMenuEntry(ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DATES, dateComp, new ArrayList<>()), cc -> {
                if (cc.getAction().isRightClick()) {
                    if (StatShops.getInstance().getShopsConfig().isConfirmDeletion()) {
                        ConfirmMenu confirmMenu = new ConfirmMenu(Message.GUI_DISCOUNT_START_DELETE_CONFIRM.getTranslation(TagResolver.resolver("date", Tag.inserting(dateComp))));
                        confirmMenu.setDenyHandler(c -> menu.openInventory(player, menu.getCurrentPage()));
                        confirmMenu.setCloseHandler(c -> menu.openInventory(player, menu.getCurrentPage()));
                        confirmMenu.setAcceptHandler(c -> {
                            discount.removeStartTime(date);
                            openDiscountStartMenu(player, discount, fromPage, menu.getCurrentPage());
                        });
                        confirmMenu.openInventory(cc.getPlayer());
                    } else {
                        discount.removeStartTime(date);
                        openDiscountStartMenu(player, discount, fromPage, menu.getCurrentPage());
                    }
                }
            });
            menu.openInventory(player, page);
        }
    }
}
