package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.Taggable;
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
import de.cubbossa.menuframework.inventory.*;
import de.cubbossa.menuframework.inventory.implementations.AnvilMenu;
import de.cubbossa.menuframework.inventory.implementations.BottomInventoryMenu;
import de.cubbossa.menuframework.inventory.implementations.ListMenu;
import de.cubbossa.menuframework.inventory.implementations.RectInventoryMenu;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public class MainMenu {


    public static MenuPreset<?> bottomRow(int row) {
        return buttonHandler -> {
            IntStream.range(row * 9, row * 9 + 9).forEach(v -> buttonHandler.addItem(v, Icon.EMPTY_DARK));
            buttonHandler.addItem(row * 9 + 5, Icon.EMPTY_DARK_RP);
        };
    }

    public static AnvilMenu newAnvilMenu(ComponentLike title, String suggestion) {
        return newAnvilMenu(title, suggestion, null);
    }

    public static <T> AnvilMenu newAnvilMenu(ComponentLike title, String suggestion, AnvilInputValidator<T> validator) {
        AnvilMenu menu = new AnvilMenu(title, suggestion);
        menu.addPreset(MenuPresets.back(1, Action.LEFT));
        menu.setClickHandler(0, AnvilMenu.WRITE, s -> {
            if (validator != null && !validator.getInputValidator().test(s.getTarget())) {
                menu.setItem(2, ItemStackUtils.createErrorItem(Message.GENERAL_GUI_WARNING_NAME.asComponent(), validator.getErrorMessage().asComponents(TagResolver.resolver("format", Tag.inserting(validator.getRequiredFormat())))));
            } else {
                menu.setItem(2, Icon.ACCEPT_RP);
            }
            menu.refresh(2);
        });
        return menu;
    }

    public static void openBaseMenu(Player player) {

        RectInventoryMenu menu = new RectInventoryMenu(Message.GUI_MAIN_TITLE, 3);
        menu.addPreset(MenuPresets.fill(Icon.EMPTY_LIGHT_RP.get()));

        // Main menu background texture
        ItemStack glass_rp = Icon.EMPTY_LIGHT.get().clone();
        ItemStackUtils.setCustomModelData(glass_rp, 7122001);
        menu.setItem(9, glass_rp);

        menu.setItemAndClickHandler(11, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
                Message.GUI_MAIN_LIMITS_NAME, Message.GUI_MAIN_LIMITS_LORE), Action.LEFT, clickContext ->
                menu.openSubMenu(clickContext.getPlayer(), newLimitsMenu()));
        menu.setItemAndClickHandler(12, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
                Message.GUI_MAIN_DISCOUNTS_NAME, Message.GUI_MAIN_DISCOUNTS_LORE), Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newDiscountsMenu()));
        menu.setItemAndClickHandler(13, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_SHOP,
                Message.GUI_MAIN_SHOPS_NAME, Message.GUI_MAIN_SHOPS_LORE), Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newShopsMenu()));
        menu.setItem(14, ItemStackUtils.createItemStack(Material.WRITABLE_BOOK,
                Message.GUI_MAIN_LANGUAGE_NAME, Message.GUI_MAIN_LANGUAGE_LORE.asComponents(TagResolver.resolver("file",
                        Tag.inserting(Component.text(StatShops.getInstance().getShopsConfig().getLanguage() + ".yml"))))));
        menu.setClickHandler(14, Action.RIGHT, clickContext -> {
            long ms = System.currentTimeMillis();
            TranslationHandler.getInstance().loadLanguage(StatShops.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
                if (success) {
                    Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getKey(),
                            Message.GENERAL_LANGUAGE_RELOADED_IN_MS.asComponent(TagResolver.resolver("ms", Tag.inserting(Component.text(System.currentTimeMillis() - ms + "")))), 0);
                    openBaseMenu(player);
                    return;
                }
                Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOAD_ERROR);
                menu.setItem(14, Icon.ERROR);
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
                customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getKey(), Message.GENERAL_WEBINTERFACE_LINK.asComponent(TagResolver.resolver("link", Tag.inserting(Component.text("https://127.0.0.1:8080/" + paste.getId())))));
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
                customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getKey(), Message.GENERAL_WEBINTERFACE_LINK.asComponent(TagResolver.resolver("link", Tag.inserting(Component.text("https://127.0.0.1:8080/" + paste.getId())))));
            });
        });
        menu.open(player);
    }

    public static TopMenu newShopsMenu() {
        int shops = ShopHandler.getInstance().getShops().size();
        ListEditorMenu<Shop> menu = new ListEditorMenu<>(Message.GUI_SHOPS_TITLE, Integer.max(3, Integer.min(shops / 9, 6)), ShopHandler.getInstance());
        menu.setClickHandler(Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newShopMenu(c.getTarget(), c.getPlayer())));
        menu.setDeleteHandler(Message.GUI_SHOPS_DELETE_CONFIRM, Action.RIGHT, c -> ShopHandler.getInstance().deleteShop(c.getTarget()));
        menu.setDuplicateHandler(Action.MIDDLE, c -> ShopHandler.getInstance().createDuplicate(c.getTarget()));
        menu.setInfoItem(Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE);
        menu.setNewHandlerStringInput(
                Message.GUI_SHOPS_NEW_NAME, Message.GUI_SHOPS_NEW_LORE,
                Message.GUI_SHOPS_NEW_TITLE, "shop name",
                s -> ShopHandler.getInstance().createShop(s.getTarget(), ChestMenuShop.class));
        return menu;
    }

    public static TopMenu newShopMenu(Shop shop, Player viewer) {

        RectInventoryMenu menu = new RectInventoryMenu(shop.getName(), 3);
        menu.addPreset(MenuPresets.fill(MenuPresets.FILLER_LIGHT));
        menu.addPreset(MenuPresets.back(2, 8, Action.LEFT));

        //Set name
        menu.setItemAndClickHandler(1, () -> ItemStackUtils.createItemStack(shop.getDisplayItem() == null ?
                        new ItemStack(ItemStackUtils.MATERIAL_SHOP) : shop.getDisplayItem(), Message.GUI_SHOP_SET_NAME_NAME, Message.GUI_SHOP_SET_NAME_LORE),
                Action.LEFT, c -> {
                    if (c.getPlayer().getItemOnCursor().getType() != Material.AIR) {
                        shop.setDisplayItem(c.getPlayer().getItemOnCursor().clone());
                        StatShops.getInstance().getDatabase().saveShop(shop);
                        c.getMenu().refresh(c.getSlot());
                        return;
                    }
                    menu.openSubMenu(c.getPlayer(), () -> {
                        AnvilMenu m = newAnvilMenu(Message.GUI_SHOP_SET_NAME_TITLE, shop.getNameFormat());
                        m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                            shop.setNameFormat(s.getTarget());
                            menu.updateTitle(shop.getName());
                            m.openPreviousMenu(s.getPlayer());
                        });
                        return m;
                    });
                });

        //Set permissions
        menu.setItemAndClickHandler(2, () -> ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
                        Message.GUI_SHOP_SET_PERMISSION_NAME, Message.GUI_SHOP_SET_PERMISSION_LORE.asComponents(
                                TagResolver.resolver("permission", Tag.inserting(Component.text(shop.getPermission() == null ? "X" : shop.getPermission()))))),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), () -> {
                    AnvilMenu m = newAnvilMenu(Message.GUI_SHOP_SET_PERMISSION_TITLE, "shops.shop." + shop.getNamePlain().toLowerCase() + ".", AnvilInputValidator.VALIDATE_PERMISSION);
                    m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                        if (!AnvilInputValidator.VALIDATE_PERMISSION.getInputValidator().test(s.getTarget())) {
                            c.getPlayer().playSound(c.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
                            return;
                        }
                        shop.setPermission(s.getTarget().trim());
                        c.getMenu().refresh(c.getSlot());
                        m.openPreviousMenu(s.getPlayer());
                    });
                    return m;
                }));

        //Open Tags menu
        menu.setItemAndClickHandler(4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
                        Message.GUI_SHOP_SET_TAGS_NAME, Message.GUI_SHOP_SET_TAGS_LORE),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newTagMenu(shop,
                        Message.GUI_TAGS_TITLE.asComponent(TagResolver.resolver("name", Tag.inserting(shop.getName()))),
                        Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
                        Message.GENERAL_GUI_TAGS_REMOVE_TAG)));

        //Open Limits menu
        List<Component> limitsLore = new ArrayList<>();
        Pair<Limit, Limit> limits = LimitsHandler.getInstance().getMinimalLimitsWithMatchingTags(null, shop);
        ItemStackUtils.addLoreLimits(limitsLore, limits.getLeft(), limits.getRight(), 0);
        if (limitsLore.size() > 0) {
            limitsLore.add(Message.SHOP_ITEM_LORE_SPACER.asComponent());
        }
        limitsLore.addAll(Message.GUI_SHOP_SET_LIMITS_LORE.asComponents());
        menu.setItemAndClickHandler(5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Message.GUI_SHOP_SET_LIMITS_NAME, limitsLore),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newShopLimitsMenu(shop)));
        ;

        //Open Discounts menu
        List<Component> discountLore = new ArrayList<>();
        List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(viewer, shop);
        ItemStackUtils.addLoreDiscount(discountLore, discounts);
        if (discountLore.size() > 0) {
            discountLore.add(Message.SHOP_ITEM_LORE_SPACER.asComponent());
        }
        discountLore.addAll(Message.GUI_SHOP_SET_DISCOUNTS_LORE.asComponents());
        menu.setItemAndClickHandler(6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_SHOP_SET_DISCOUNTS_NAME, discountLore),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newShopDiscountsMenu(shop)));

        if (shop instanceof TemplatableShop tShop) {
            //Open Templates menu
            menu.setItemAndClickHandler(7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
                            Message.GUI_SHOP_SET_TEMPLATE_NAME, Message.GUI_SHOP_SET_TEMPLATE_LORE.asComponents(TagResolver.resolver("template",
                                    Tag.inserting(tShop.getDefaultTemplate() == null ? Component.text("none") : tShop.getDefaultTemplate().getName())))),
                    Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newDefaultTemplateMenu(tShop)));
        }

        //Assign Shop to NPC
        if (StatShops.getInstance().isCitizensInstalled()) {

            List<String> assigned = StatShops.getInstance().getCitizensHook().getAssignedNPCs(shop);
            menu.setItemAndClickHandler(8, ItemStackUtils.createItemStack(Material.PLAYER_HEAD,
                            Message.GUI_SHOP_SET_NPC_NAME, Message.GUI_SHOP_SET_NPC_LORE.asComponents(
                                    TagResolver.resolver("current", Tag.inserting(Component.text(assigned.isEmpty() ?
                                            "none" : String.join("<gray>, </gray>", assigned)))))),
                    Action.LEFT, c -> {
                        StatShops.getInstance().getCitizensHook().addAssigningPlayer(c.getPlayer(), shop);
                        menu.openPreviousMenu(c.getPlayer());
                        Customer.wrap(c.getPlayer()).sendMessage(Message.CITIZENS_CLICK_TO_ASSIGN);
                    });
        }

        menu.setItemAndClickHandler(9 + 1, ItemStackUtils.createItemStack(Material.SMITHING_TABLE,
                        Message.GUI_SHOP_SET_CONTENT_NAME, Message.GUI_SHOP_SET_CONTENT_LORE),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), () -> {
                    TopMenu m = shop.newEditorMenu();
                    m.setOpenHandler(x);
                    BottomMenu bottom = new BottomInventoryMenu(InventoryRow.FIRST_ROW);
                    bottom.addPreset(MenuPresets.fill(Icon.EMPTY_DARK.create()));
                    m.setCloseHandler(x);
                }));

        menu.setItemAndClickHandler(9 + 2, ItemStackUtils.createItemStack(Material.CHEST,
                Message.GUI_SHOP_SET_PREVIEW_NAME, Message.GUI_SHOP_SET_PREVIEW_LORE), Action.LEFT, c -> {

            //TODO bottom inventory
            shop.open(Customer.wrap(c.getPlayer()));
        });


        if (shop instanceof PaginatedShop ps) {

            // default page
            menu.setButton(22, Button.builder()
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

            menu.setButton(24, Button.builder()
                    .withItemStack(rememberPage.get())
                    .withClickHandler(Action.LEFT, c -> {
                        ps.setPageRemembered(!ps.isPageRemembered());
                        c.getMenu().setItem(24, rememberPage.get());
                        c.getMenu().refresh(c.getSlot());
                    }));
        }

        if (shop instanceof ChestMenuShop chestMenuShop) {

            // rows
            menu.setButton(23, Button.builder()
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
        lore.add(Message.GUI_SHOP_SET_ROWS_LORE.asComponent(TagResolver.resolver("rows", Tag.inserting(Component.text("" + row)))));

        return ItemStackUtils.createItemStack(new ItemStack(Material.RAIL, row), Message.GUI_SHOP_SET_ROWS_NAME.asComponent(
                TagResolver.resolver("rows", Tag.inserting(Component.text("" + row)))), lore);
    }

    private static ItemStack getDefaultPageItem(PaginatedShop shop, int page) {
        int pageCount = shop.getPageCount();
        List<Component> lore = new ArrayList<>();
        lore.add(Message.GUI_SHOP_SET_DEFAULT_PAGE_LORE.asComponent(TagResolver.resolver("page", Tag.inserting(Component.text("" + (page + 1)))), TagResolver.resolver("pages", Tag.inserting(Component.text("" + pageCount)))));
        return ItemStackUtils.createItemStack(new ItemStack(Material.BOOK, Integer.min(Integer.max(page + 1, 1), 127)),
                Message.GUI_SHOP_SET_DEFAULT_PAGE_NAME.asComponent(TagResolver.resolver("page", Tag.inserting(Component.text("" + (page + 1)))), TagResolver.resolver("pages", Tag.inserting(Component.text("" + pageCount)))), lore);
    }

    public static TopMenu newShopLimitsMenu(Shop shop) {
        return newShopTaggableMenu(shop, LimitsHandler.getInstance(),
                Message.GUI_SHOP_LIMITS_TITLE, 3, Message.GUI_SHOP_LIMITS_INFO_NAME, Message.GUI_SHOP_LIMITS_INFO_LORE);
    }

    public static TopMenu newShopDiscountsMenu(Shop shop) {
        return newShopTaggableMenu(shop, DiscountHandler.getInstance(),
                Message.GUI_SHOP_DISCOUNTS_TITLE, 3, Message.GUI_SHOP_DISCOUNTS_INFO_NAME, Message.GUI_SHOP_DISCOUNTS_INFO_LORE);
    }

    public static TopMenu newDefaultTemplateMenu(TemplatableShop shop) { //TODO refresh icon
        ListMenu menu = new ListMenu(Message.GUI_SHOP_TEMPLATE_TITLE, 3);

        menu.addPreset(bottomRow(2));
        menu.addPreset(MenuPresets.paginationRow(2, 0, 1, false, Action.LEFT));
        menu.addPreset(MenuPresets.back(2 * 9 + 8, Action.LEFT));
        menu.addPreset(presetApplier -> presetApplier.addItem(2 * 9 + 4, ItemStackUtils.createInfoItem(Message.GUI_SHOP_TEMPLATE_INFO_NAME, Message.GUI_SHOP_TEMPLATE_INFO_LORE)));

        for (EntryTemplate template : TemplateHandler.getInstance().getTemplates()) {
            menu.addListEntry(Button.builder()
                    .withItemStack(() -> {
                        ItemStack stack = TemplateHandler.getInstance().getDisplayItem(template);
                        return shop.getDefaultTemplate() != null && template.equals(shop.getDefaultTemplate()) ? ItemStackUtils.setGlow(stack) : stack;
                    })
                    .withClickHandler(Action.LEFT, c -> {
                        if (shop.getDefaultTemplate() != null && template.equals(shop.getDefaultTemplate())) {
                            shop.setDefaultTemplate(null);
                            c.getMenu().refresh(c.getSlot());
                        } else {
                            shop.setDefaultTemplate(template);
                            c.getMenu().refresh(c.getMenu().getSlots());
                        }
                    })
                    .withClickHandler(Action.RIGHT, c -> {
                        shop.setDefaultTemplate(null);
                        c.getMenu().refresh(c.getSlot());
                    }));
        }
        return menu;
    }

    public static ListMenu newLimitsMenu() {
        ListEditorMenu<Limit> menu = new ListEditorMenu<>(Message.GUI_LIMITS, 3, LimitsHandler.getInstance());
        menu.setClickHandler(Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newLimitMenu(c.getTarget())));
        menu.setDeleteHandler(Message.GUI_LIMITS_DELETE_CONFIRM, Action.RIGHT, c -> LimitsHandler.getInstance().delete(c.getTarget()));
        menu.setDuplicateHandler(Action.MIDDLE, c -> LimitsHandler.getInstance().createDuplicate(c.getTarget()));
        menu.setInfoItem(Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE);
        menu.setNewHandlerStringInput(
                Message.GUI_LIMITS_NEW_NAME, Message.GUI_LIMITS_NEW_LORE,
                Message.GUI_LIMITS_NEW_TITLE, "new limit",
                s -> LimitsHandler.getInstance().createNew(s.getTarget()));
        return menu;
    }

    public static TopMenu newLimitMenu(Limit limit) {

        RectInventoryMenu menu = new RectInventoryMenu(limit.getName(), 3);
        menu.setCloseHandler(closeContext -> limit.saveToDatabase());
        menu.addPreset(MenuPresets.fill(MenuPresets.FILLER_LIGHT));
        menu.addPreset(MenuPresets.back(2, 8, Action.LEFT));

        //Set name
        menu.setItemAndClickHandler(1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
                Message.GUI_LIMIT_SET_NAME_NAME, Message.GUI_LIMIT_SET_NAME_LORE), Action.LEFT, c -> {
            AnvilMenu m = newAnvilMenu(Message.GUI_LIMIT_SET_NAME_TITLE, limit.getNameFormat());
            m.setOutputClickHandler(AnvilMenu.CONFIRM, clickContext -> limit.setNameFormat(clickContext.getTarget()));
            menu.openSubMenu(c.getPlayer(), m);
        });
        //Set permissions
        menu.setItemAndClickHandler(2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
                Message.GUI_LIMIT_SET_PERMISSION_NAME,
                Message.GUI_LIMIT_SET_PERMISSION_LORE.asComponents(TagResolver.resolver("permission",
                        Tag.inserting(Component.text(limit.getPermission() == null ? "X" : limit.getPermission())))
                )), Action.LEFT, c -> {

            AnvilMenu m = newAnvilMenu(Message.GUI_LIMIT_SET_PERMISSION_TITLE, "shops.limit." + limit.getNamePlain().toLowerCase() + ".", AnvilInputValidator.VALIDATE_PERMISSION);
            m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                if (!AnvilInputValidator.VALIDATE_PERMISSION.getInputValidator().test(s.getTarget())) {
                    s.getPlayer().playSound(s.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
                    return;
                }
                limit.setPermission(s.getTarget().trim());
            });
            menu.openSubMenu(c.getPlayer(), m);
        });
        //Open Tags menu
        menu.setItemAndClickHandler(4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
                        Message.GUI_LIMIT_SET_TAGS_NAME, Message.GUI_LIMIT_SET_TAGS_LORE), Action.LEFT,
                clickContext -> menu.openSubMenu(clickContext.getPlayer(),
                        newTagMenu(limit, Message.GUI_TAGS_TITLE.asComponent(TagResolver.resolver("name", Tag.inserting(limit.getName()))),
                                Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE, Message.GENERAL_GUI_TAGS_REMOVE_TAG)));

        Supplier<ItemStack> durationStack = () -> ItemStackUtils.createItemStack(Material.COMPASS, Message.GUI_LIMIT_SET_DURATION_NAME,
                Message.GUI_LIMIT_SET_DURATION_LORE.asComponents(TagResolver.resolver("current", Tag.inserting(Component.text(TextUtils.formatDuration(limit.getRecover()))))));
        menu.setItemAndClickHandler(5, durationStack.get(), Action.LEFT, clickContext -> {
            menu.openSubMenu(clickContext.getPlayer(), () -> {
                AnvilMenu m = newAnvilMenu(Message.GUI_LIMIT_SET_DURATION_TITLE, TextUtils.DURATION_FORMAT, AnvilInputValidator.VALIDATE_DURATION);
                m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                    Player player = s.getPlayer();
                    if (!AnvilInputValidator.VALIDATE_DURATION.getInputValidator().test(s.getTarget())) {
                        player.playSound(player.getLocation(), Sound.ENTITY_VILLAGER_NO, .5f, 1f);
                        return;
                    }
                    Duration duration = TextUtils.parseDuration(s.getTarget().trim());
                    limit.setRecover(duration);
                    menu.setItem(5, durationStack.get());
                    menu.refresh(5);
                    m.openPreviousMenu(s.getPlayer());
                });
                return m;
            });
        });

        Supplier<ItemStack> limitStack = () -> ItemStackUtils.createItemStack(Material.PAPER, Message.GUI_LIMIT_SET_LIMIT_NAME,
                Message.GUI_LIMIT_SET_LIMIT_LORE.asComponents(TagResolver.resolver("current", Tag.inserting(Component.text(limit.getTransactionLimit() + "")))));

        menu.setItemAndClickHandler(6, limitStack.get(), Action.LEFT, clickContext -> {
            menu.openSubMenu(clickContext.getPlayer(), () -> {
                AnvilMenu m = newAnvilMenu(Message.GUI_LIMIT_SET_LIMIT_TITLE, "64 ", AnvilInputValidator.VALIDATE_INT);
                m.setOutputClickHandler(AnvilMenu.CONFIRM, c -> {
                    Player player = c.getPlayer();
                    if (!AnvilInputValidator.VALIDATE_INT.getInputValidator().test(c.getTarget())) {
                        player.playSound(player.getLocation(), Sound.ENTITY_VILLAGER_NO, .5f, 1f);
                        return;
                    }
                    limit.setTransactionLimit(Integer.parseInt(c.getTarget().trim()));
                    menu.setItem(6, limitStack.get());
                    menu.refresh(6);
                    menu.openPreviousMenu(c.getPlayer());

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

    public static TopMenu newDiscountsMenu() {
        ListEditorMenu<Discount> menu = new ListEditorMenu<>(Message.GUI_DISCOUNTS, 3, DiscountHandler.getInstance());
        menu.setClickHandler(Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newDiscountMenu(c.getTarget())));
        menu.setDeleteHandler(Message.GUI_DISCOUNTS_DELETE_CONFIRM, Action.RIGHT, c -> DiscountHandler.getInstance().delete(c.getTarget()));
        menu.setDuplicateHandler(Action.MIDDLE, c -> DiscountHandler.getInstance().createDuplicate(c.getTarget()));
        menu.setInfoItem(Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE);
        menu.setNewHandlerStringInput(
                Message.GUI_DISCOUNTS_NEW_NAME, Message.GUI_DISCOUNTS_NEW_LORE,
                Message.GUI_DISCOUNTS_NEW_TITLE, "type name",
                s -> DiscountHandler.getInstance().createNew(s.getTarget()));
        return menu;
    }

    public static TopMenu newDiscountMenu(Discount discount) {
        RectInventoryMenu menu = new RectInventoryMenu(Message.GUI_DISCOUNT, 3);

        menu.addPreset(MenuPresets.fill(MenuPresets.FILLER_LIGHT));
        menu.addPreset(MenuPresets.back(2, 8, Action.LEFT));
        menu.setCloseHandler(closeContext -> discount.saveToDatabase());

        //Set name
        menu.setItemAndClickHandler(1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_DISCOUNT_SET_NAME_NAME, Message.GUI_DISCOUNT_SET_NAME_LORE),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), () -> {
            AnvilMenu m = newAnvilMenu(Message.GUI_DISCOUNT_SET_NAME_TITLE, discount.getNameFormat());
            m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                discount.setNameFormat(s.getTarget());
                menu.updateTitle(discount.getName());
                m.openPreviousMenu(s.getPlayer());
            });
            return m;
        }));

        //Set permissions
        menu.setItemAndClickHandler(2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS, Message.GUI_DISCOUNT_SET_PERMISSION_NAME,
                        Message.GUI_DISCOUNT_SET_PERMISSION_LORE.asComponents(TagResolver.resolver("permission", Tag.inserting(Component.text(discount.getPermission() == null ? "X" : discount.getPermission()))))),
                Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), () -> {
                    AnvilMenu m = newAnvilMenu(Message.GUI_DISCOUNT_SET_PERMISSION_TITLE, "shops.discount." + discount.getNamePlain().toLowerCase() + ".", AnvilInputValidator.VALIDATE_PERMISSION);
                    m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                        if (!AnvilInputValidator.VALIDATE_PERMISSION.getInputValidator().test(s.getTarget())) {
                            s.getPlayer().playSound(s.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
                            return;
                        }
                        discount.setPermission(s.getTarget().trim());
                        c.getMenu().refresh(c.getSlot());
                        m.openPreviousMenu(s.getPlayer());
                    });
                    return m;
                }));

        //Open Tags menu
        menu.setItemAndClickHandler(4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
                Message.GUI_DISCOUNT_SET_TAGS_NAME, Message.GUI_DISCOUNT_SET_TAGS_LORE), Action.LEFT, c -> menu.openSubMenu(c.getPlayer(),
                newTagMenu(discount, Message.GUI_TAGS_TITLE.asComponent(TagResolver.resolver("name", Tag.inserting(discount.getName()))),
                        Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
                        Message.GENERAL_GUI_TAGS_REMOVE_TAG)));

        menu.setItemAndClickHandler(5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DATES, Message.GUI_DISCOUNT_SET_START_NAME,
                Message.GUI_DISCOUNT_SET_START_LORE), Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), newDiscountStartMenu(discount)));

        TagResolver dur = TagResolver.resolver("duration", Tag.inserting(Component.text(TextUtils.formatDuration(discount.getDuration()))));
        menu.setItemAndClickHandler(6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DURATIONS, Message.GUI_DISCOUNT_SET_DURATION_NAME.asComponent(dur),
                Message.GUI_DISCOUNT_SET_DURATION_LORE.asComponents(dur)), Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), () -> {
            AnvilMenu m = newAnvilMenu(Message.GUI_DISCOUNT_SET_PERMISSION_TITLE, TextUtils.DURATION_FORMAT, AnvilInputValidator.VALIDATE_DURATION);
            m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                if (!AnvilInputValidator.VALIDATE_DURATION.getInputValidator().test(s.getTarget())) {
                    s.getPlayer().playSound(s.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
                    return;
                }
                discount.setDuration(TextUtils.parseDuration(s.getTarget().trim()));
                c.getMenu().refresh(c.getSlot());
                m.openPreviousMenu(s.getPlayer());
            });
            return m;
        }));

        TagResolver percent = TagResolver.resolver("percent", Tag.inserting(discount.getFormattedPercent()));
        menu.setItemAndClickHandler(7, ItemStackUtils.createItemStack(Material.EMERALD, Message.GUI_DISCOUNT_SET_PERCENT_NAME.asComponent(percent),
                Message.GUI_DISCOUNT_SET_PERCENT_LORE.asComponents(percent)), Action.LEFT, c -> menu.openSubMenu(c.getPlayer(), () -> {
            AnvilMenu m = newAnvilMenu(Message.GUI_DISCOUNT_SET_PERCENT_TITLE, "5%", AnvilInputValidator.VALIDATE_PERCENT);
            m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
                if (!AnvilInputValidator.VALIDATE_PERCENT.getInputValidator().test(s.getTarget())) {
                    s.getPlayer().playSound(s.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
                    return;
                }
                double d = Double.parseDouble(s.getTarget().replace("%", "").trim());
                discount.setPercent(d);
                c.getMenu().refresh(c.getSlot());
                m.openPreviousMenu(s.getPlayer());
            });
            return m;
        }));

        return menu;
    }

    public static TopMenu newDiscountStartMenu(Discount discount) {
        ListEditorMenu<LocalDateTime> m = new ListEditorMenu<>(Message.GUI_DISCOUNT_SET_START_TITLE, 3, new ListMenuSupplier<LocalDateTime>() {

            public Collection<LocalDateTime> getElements() {
                return discount.getStartTimes();
            }

            public ItemStack getDisplayItem(LocalDateTime object) {
                Component dateComp = Component.text(TextUtils.formatLocalDateTime(object), NamedTextColor.WHITE);
                return ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DATES, dateComp, new ArrayList<>());
            }
        });
        m.setInfoItem(Message.GUI_DISCOUNT_START_INFO_NAME, Message.GUI_DISCOUNT_START_INFO_LORE);
        m.setDeleteHandler(Action.RIGHT, c -> discount.removeStartTime(c.getTarget()));
        m.setNewHandlerStringInput(Message.GUI_DISCOUNT_START_NEW_NAME, Message.GUI_DISCOUNT_START_NEW_LORE,
                Message.GUI_DISCOUNT_START_NEW_TITLE, TextUtils.DATE_TIME_FORMAT,
                AnvilInputValidator.VALIDATE_DATE_TIME, s -> discount.addStartTime(TextUtils.parseLocalDateTime(s.getTarget())));
        return m;
    }

    public static <T extends Taggable> ListMenu newShopTaggableMenu(Shop shop, ListMenuSupplier<T> taggableSupplier, ComponentLike title, int rows,
                                                                    Message infoName, Message infoLore) {
        ListMenu menu = new ListMenu(title, rows);

        menu.addPreset(bottomRow(2));
        menu.addPreset(presetApplier -> presetApplier.addItem(2 * 9 + 4, ItemStackUtils.createInfoItem(infoName, infoLore)));
        menu.addPreset(MenuPresets.paginationRow(2, 0, 1, false, Action.LEFT));
        menu.addPreset(MenuPresets.back(2, 8, Action.LEFT));

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
        ListEditorMenu<String> menu = new ListEditorMenu<>(title, 3, new ListMenuSupplier<>() {

            public Collection<String> getElements() {
                return taggable.getTags();
            }

            public ItemStack getDisplayItem(String object) {
                return ItemStackUtils.createItemStack(Material.NAME_TAG, Component.text(object, NamedTextColor.WHITE), new ArrayList<>());
            }
        });
        menu.setInfoItem(Message.GENERAL_GUI_TAGS_INFO_NAME, Message.GENERAL_GUI_TAGS_INFO_LORE);
        menu.setNewHandlerStringInput(newTagName, newTagLore, newTagTitle, "tag-me", new AnvilInputValidator<>(
                        Message.ERROR_PARSE_STRING,
                        Component.text("<unique tag>"),
                        s -> !taggable.hasTag(s.toLowerCase().trim()),
                        String::trim),
                c -> taggable.addTag(c.getTarget()));
        menu.setDeleteHandler(confirmRemove, Action.RIGHT, s -> taggable.removeTag(s.getTarget()));
        return menu;
    }
}
