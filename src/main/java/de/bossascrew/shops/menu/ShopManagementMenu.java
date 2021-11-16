package de.bossascrew.shops.menu;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Config;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.CustomerHandler;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.handler.ShopHandler;
import de.bossascrew.shops.handler.TranslationHandler;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;

public class ShopManagementMenu {

	private final ClickType delete;

	public ShopManagementMenu() {
		Config sc = ShopPlugin.getInstance().getShopsConfig();
		delete = sc.getKeyBindDelete();
	}


	public void openBaseMenu(Player player) {
		ChestMenu chestMenu = new ChestMenu(Message.MANAGER_GUI_MAIN_TITLE, 3);

		chestMenu.setItemAndClickHandler(1, 1, DefaultSpecialItem.MANAGER_MAIN_SHOPS, clickContext -> openShopsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 2, DefaultSpecialItem.MANAGER_MAIN_LIMITS, clickContext -> openLimitsMenu(player));
		chestMenu.setItemAndClickHandler(1, 3, DefaultSpecialItem.MANAGER_MAIN_DISCOUNTS, clickContext -> openDiscountsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 6, ItemStackUtils.createItemStack(Material.BOOK,
				Message.MANAGER_GUI_MAIN_LANGUAGE_NAME, Message.MANAGER_GUI_MAIN_LANGUAGE_LORE), clickContext -> {
			ShopPlugin.getInstance().getShopsConfig().reloadLanguage();
			TranslationHandler.getInstance().loadLanguage(ShopPlugin.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
				if (success) {
					openBaseMenu(player);
					return;
				}
				chestMenu.setItem(1, 6, DefaultSpecialItem.ERROR.createSpecialItem());
			});
		});
		chestMenu.setItemAndClickHandler(1, 7, DefaultSpecialItem.MANAGER_MAIN_WEBINTERFACE, clickContext -> {
			player.sendMessage("Not yet implemented - but Test:");
			//TODO generate interface link and send to player
			ShopPlugin.getInstance().runAsync(() -> player.sendMessage("https://127.0.0.1:"));

			player.closeInventory();
		});
		chestMenu.openInventory(player);
	}

	public void openShopsMenu(Player player, int page) {
		PagedChestMenu chestMenu = new PagedChestMenu(Message.MANAGER_GUI_SHOPS_TITLE.getTranslation(), 4, null, null, backContext -> {
			openBaseMenu(player);
		});

		for (Shop shop : ShopHandler.getInstance().getShops()) {
			chestMenu.addMenuEntry(ItemStackUtils.createShopItemStack(shop), clickContext -> {
				if (shop.getEditor() != null) {
					CustomerHandler.getInstance().getCustomer(player).sendMessage(Message.MANAGER_GUI_SHOPS_ALREADY_EDITED.getTranslation(
							Template.of("player", shop.getEditor().getName())));
					return;
				}
				if (clickContext.getAction().equals(delete)) {
					ConfirmMenu confirmMenu = new ConfirmMenu(Message.MANAGER_GUI_SHOPS_DELETE_CONFIRM
							.getTranslation(Template.of("shop", shop.getName())), backContext -> openShopsMenu(player, chestMenu.getCurrentPage()));
					confirmMenu.setAcceptHandler(clickContext1 -> {
						ShopHandler.getInstance().deleteShop(shop);
						openShopsMenu(player, chestMenu.getCurrentPage());
					});
					confirmMenu.setDenyHandler(clickContext1 -> {
						openShopsMenu(player, chestMenu.getCurrentPage());
					});
					confirmMenu.openInventory(player);
				} else {
					openShopMenu(player, shop, chestMenu.getCurrentPage());
				}
			});
		}
		chestMenu.setNavigationEntry(7, DefaultSpecialItem.MANAGER_SHOPS_NEW.createSpecialItem(), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("name")
					.title(Message.MANAGER_GUI_SHOPS_NEW_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openShopsMenu(p, chestMenu.getCurrentPage()), 1L))
					.onComplete((p, s) -> {
						ShopHandler.getInstance().createShop(s);
						openShopsMenu(player, chestMenu.getCurrentPage());
						return AnvilGUI.Response.close();
					}).open(player);
		});
		chestMenu.openInventory(player, page);
	}

	public void openShopMenu(Player player, Shop shop, int fromPage) {

		//deactivate shop so we can change stuff without worrying about players messing things up
		//the shop will activate itself automatically when using the back button. Otherwise it needs to be reactivated manually
		shop.setEnabled(false);
		shop.setEditor(player);

		ChestMenu chestMenu = new ChestMenu(shop.getName(), 3);
		//Set name
		chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_SHOP,
				Message.MANAGER_GUI_SHOP_SET_NAME_NAME, Message.MANAGER_GUI_SHOP_SET_NAME_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text(shop.getNameFormat())
					.title(Message.MANAGER_GUI_SHOP_SET_NAME_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openShopMenu(p, shop, fromPage), 1L))
					.onComplete((p, s) -> {
						shop.setNameFormat(s);
						openShopMenu(player, shop, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.MANAGER_GUI_SHOP_SET_PERMISSION_NAME.getTranslation(), Message.MANAGER_GUI_SHOP_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", shop.getPermission() == null ? "X" : shop.getPermission())
				)), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("shop.open." + shop.getNamePlain().toLowerCase() + ".")
					.title(Message.MANAGER_GUI_SHOP_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openShopMenu(p, shop, fromPage), 1L))
					.onComplete((p, s) -> {
						shop.setPermission(s);
						openShopMenu(player, shop, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.MANAGER_GUI_SHOP_SET_TAGS_NAME, Message.MANAGER_GUI_SHOP_SET_TAGS_LORE),
				clickContext -> openShopTagsMenu(player, shop, fromPage, 0));
		//Open Limits menu
		chestMenu.setItemAndClickHandler(0, 5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
						Message.MANAGER_GUI_SHOP_SET_LIMITS_NAME, Message.MANAGER_GUI_SHOP_SET_LIMITS_LORE),
				clickContext -> openShopLimitsMenu(player, shop, 0));
		//Open Discounts menu
		chestMenu.setItemAndClickHandler(0, 6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
						Message.MANAGER_GUI_SHOP_SET_DISCOUNTS_NAME, Message.MANAGER_GUI_SHOP_SET_DISCOUNTS_LORE),
				clickContext -> openShopDiscountsMenu(player, shop, 0));

		chestMenu.setItemAndClickHandler(0, 8, ItemStackUtils.createItemStack(Material.BARRIER,
				Message.GENERAL_GUI_DELETE_NAME, Message.GENERAL_GUI_DELETE_LORE), clickContext -> {
			ConfirmMenu confirmMenu = new ConfirmMenu(Message.MANAGER_GUI_SHOPS_DELETE_CONFIRM
					.getTranslation(Template.of("shop", shop.getName())), backContext -> openShopMenu(player, shop, fromPage));
			confirmMenu.setCloseHandler(closeContext -> {
				shop.setEnabled(true); //TODO nur wenn booleansetting verändert wurde
				shop.setEditor(null);
			});
			confirmMenu.setAcceptHandler(clickContext1 -> {
				ShopHandler.getInstance().deleteShop(shop);
				openShopsMenu(player, 0);
			});
			confirmMenu.setDenyHandler(clickContext1 -> openShopMenu(player, shop, fromPage));
			confirmMenu.openInventory(player);
		});

		chestMenu.setItemAndClickHandler(1, 1, ItemStackUtils.createItemStack(Material.SMITHING_TABLE,
				Message.MANAGER_GUI_SHOP_SET_CONTENT_NAME, Message.MANAGER_GUI_SHOP_SET_CONTENT_LORE), clickContext -> {
			if (shop instanceof ChestMenuShop chestMenuShop) {
				openShopEditor(player, chestMenuShop, fromPage, shop.getDefaultShopMode(), shop.getDefaultShopPage());
			} else {
				player.sendMessage("not possible due to shop type"); //TODO
			}
		});
		chestMenu.setItemAndClickHandler(1, 2, ItemStackUtils.createItemStack(Material.CHEST,
				Message.MANAGER_GUI_SHOP_SET_PREVIEW_NAME, Message.MANAGER_GUI_SHOP_SET_PREVIEW_LORE), clickContext -> {
			shop.open(Customer.wrap(player), backContext -> openShopMenu(player, shop, fromPage));
		});

		//Shopmode switch button
		chestMenu.setItem(19, getDefaultModeItem(shop.getDefaultShopMode()));
		chestMenu.setClickHandler(19, clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				shop.setDefaultShopMode(shop.getDefaultShopMode().getPrevious());
			} else if (clickContext.getAction().isLeftClick()) {
				shop.setDefaultShopMode(shop.getDefaultShopMode().getNext());
			}
			chestMenu.setItem(19, getDefaultModeItem(shop.getDefaultShopMode()));
			chestMenu.refresh(19);
		});
		chestMenu.setItemAndClickHandler(20, getDefaultPageItem(shop, shop.getDefaultShopPage()), clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				shop.setDefaultShopPage((shop.getDefaultShopPage() - 1) % shop.getPageCount());
			} else if (clickContext.getAction().isLeftClick()) {
				shop.setDefaultShopPage((shop.getDefaultShopPage() + 1) % shop.getPageCount());
			}
			chestMenu.setItem(20, getDefaultPageItem(shop, shop.getDefaultShopPage()));
			chestMenu.refresh(20);
		});
		if (shop instanceof ChestMenuShop chestMenuShop) {
			chestMenu.setItemAndClickHandler(21, getRowsItem(chestMenuShop.getRows()), clickContext -> {
				if (clickContext.getAction().isRightClick()) {
					chestMenuShop.setRows(chestMenuShop.getRows() - 1);
				} else if (clickContext.getAction().isLeftClick()) {
					chestMenuShop.setRows(chestMenuShop.getRows() + 1);
				}
				chestMenu.setItem(21, getRowsItem(chestMenuShop.getRows()));
				chestMenu.refresh(21);
			});
		}

		//Set shop enabled
		ItemStack shopEnabled = getButton(shop.isEnabled(),
				Message.MANAGER_GUI_SHOP_SET_ENABLED_NAME, Message.MANAGER_GUI_SHOP_SET_ENABLED_LORE);
		chestMenu.setItemAndClickHandler(22, shopEnabled, clickContext -> {
			shop.setEnabled(!shop.isEnabled());
			chestMenu.setItem(22, getButton(shop.isEnabled(),
					Message.MANAGER_GUI_SHOP_SET_ENABLED_NAME, Message.MANAGER_GUI_SHOP_SET_ENABLED_LORE));
			chestMenu.refresh(22);
		});
		//Set page remembered
		ItemStack rememberPage = getButton(shop.isPageRemembered(),
				Message.MANAGER_GUI_SHOP_SET_REMEMBER_PAGE_NAME, Message.MANAGER_GUI_SHOP_SET_REMEMBER_PAGE_LORE);
		chestMenu.setItemAndClickHandler(23, rememberPage, clickContext -> {
			shop.setPageRemembered(!shop.isPageRemembered());
			chestMenu.setItem(23, getButton(shop.isPageRemembered(),
					Message.MANAGER_GUI_SHOP_SET_REMEMBER_PAGE_NAME, Message.MANAGER_GUI_SHOP_SET_REMEMBER_PAGE_LORE));
			chestMenu.refresh(23);
		});
		//Set mode remembered
		ItemStack rememberMode = getButton(shop.isModeRemembered(),
				Message.MANAGER_GUI_SHOP_SET_REMEMBER_MODE_NAME, Message.MANAGER_GUI_SHOP_SET_REMEMBER_MODE_LORE);
		chestMenu.setItemAndClickHandler(24, rememberMode, clickContext -> {
			shop.setModeRemembered(!shop.isModeRemembered());
			chestMenu.setItem(24, getButton(shop.isModeRemembered(),
					Message.MANAGER_GUI_SHOP_SET_REMEMBER_MODE_NAME, Message.MANAGER_GUI_SHOP_SET_REMEMBER_MODE_LORE));
			chestMenu.refresh(24);
		});


		chestMenu.setBackSlot(26);
		chestMenu.setBackHandlerAction(backContext -> {
			shop.setEnabled(true);
			shop.setEditor(null);
			openShopsMenu(player, fromPage);
		});
		chestMenu.setCloseHandler(closeContext -> {
			shop.setEnabled(true);
			shop.setEditor(null);
		});
		chestMenu.openInventory(player);
	}

	private ItemStack getRowsItem(int row) {
		List<Component> lore = new ArrayList<>();
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		int prev = row - 1 % 6;
		if(prev <= 0) {
			prev +=6;
		}
		int next = row + 1 % 6;
		if(next <= 0) {
			next +=6;
		}
		if(row == 0) {
			row = 6;
		}
		lore.add(Message.MANAGER_GUI_SHOP_SET_ROWS_LORE.getTranslation(Template.of("rows", "" + prev)));
		lore.add(Message.MANAGER_GUI_SHOP_SET_ROWS_LORE.getTranslation(Template.of("rows", "" + row)));
		lore.add(Message.MANAGER_GUI_SHOP_SET_ROWS_LORE.getTranslation(Template.of("rows", "" + next)));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));

		return ItemStackUtils.createItemStack(Material.RAIL, Message.MANAGER_GUI_SHOP_SET_ROWS_NAME.getTranslation(
				Template.of("rows", "" + row)), lore);
	}

	private ItemStack getDefaultModeItem(ShopMode shopMode) {
		ItemStack modeItem = shopMode.getDisplayItem();
		List<Component> lore = new ArrayList<>();
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getPrevious().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getDisplayName())));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getNext().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		ItemStack item = ItemStackUtils.createItemStack(modeItem.getType(),
				Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_NAME.getTranslation(Template.of("name", modeItem.getItemMeta().getDisplayName())),
				lore);
		return item;
	}

	private ItemStack getDefaultPageItem(Shop shop, int page) {
		int pageCount = shop.getPageCount();
		List<Component> lore = new ArrayList<>();
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(Template.of("page", "" + ((page - 1) % pageCount))));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(Template.of("page", "" + page)));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(Template.of("page", "" + ((page + 1) % pageCount))));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		return ItemStackUtils.createItemStack(Material.BOOK,
				Message.MANAGER_GUI_SHOP_SET_DEFAULT_PAGE_NAME.getTranslation(Template.of("page", "" + page)), lore);
	}

	private ItemStack getButton(boolean val, Message name, Message lore) {
		return ItemStackUtils.createItemStack(val ? Material.LIME_DYE : Material.GRAY_DYE,
				name.getTranslation(Template.of("value", val + "")),
				lore.getTranslations(Template.of("value", val + "")));
	}

	public void openShopEditor(Player player, ChestMenuShop shop, int fromPage, ShopMode shopMode, int shopPage) {
		ShopEditorMenu menu = new ShopEditorMenu(Message.SHOP_GUI_TITLE.getTranslation(
				Template.of("name", shop.getName()),
				Template.of("page", "" + (shopPage + 1)),
				Template.of("pages", "" + Integer.max(shop.getPageCount(), shopPage + 1)),
				Template.of("mode", shopMode.getDisplayName())), shop.getRows(), 1);
		menu.fillMenu(DefaultSpecialItem.EMPTY_LIGHT);
		for (int i = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE; i < (shopPage + 1) * RowedOpenableMenu.LARGEST_INV_SIZE; i++) {
			ShopEntry entry = shop.getEntry(shopMode, i);
			if (entry == null) {
				continue;
			}
			menu.setItem(i - shopPage * RowedOpenableMenu.LARGEST_INV_SIZE, ItemStackUtils.prepareEditorEntryItemStack(entry, false));
		}
		menu.setDefaultClickHandler(ClickType.LEFT, clickContext -> {
			ShopEntry oldEntry = shop.getEntry(shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);

			//TODO zu umständlich gedacht. Stattdessen nbttag setzen wenn item reingelegt
			if (clickContext.getPlayer().getItemOnCursor() == null) {
				menu.setSelectedEntry(oldEntry);
				clickContext.setItemStack(ItemStackUtils.prepareEditorEntryItemStack(oldEntry, true));
			} else {
				ItemStack temp = clickContext.getPlayer().getItemOnCursor();
				ShopEntry entry = shop.createEntry(temp, shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
				clickContext.getPlayer().setItemOnCursor(oldEntry == null ? null : oldEntry.getDisplayItem());
				menu.setSelectedEntry(entry);
				clickContext.setItemStack(ItemStackUtils.prepareEditorEntryItemStack(entry, true));
			}
		});
		menu.setDefaultClickHandler(ClickType.RIGHT, clickContext -> {
			shop.deleteEntry(shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
			clickContext.setItemStack(DefaultSpecialItem.EMPTY_LIGHT.createSpecialItem());
			menu.refresh(clickContext.getSlot());
		});
		menu.fillBottom();
		menu.setBackSlotBottom(8);
		menu.setBackHandlerAction(backContext -> openShopMenu(player, shop, fromPage));
		menu.setItemAndClickHandlerBottom(0, 0, shopPage > 0 ? DefaultSpecialItem.PREV_PAGE : DefaultSpecialItem.PREV_PAGE_OFF, clickContext -> {
			openShopEditor(player, shop, fromPage, shopMode, shopPage > 0 ? shopPage - 1 : shopPage);
		});
		menu.setItemAndClickHandlerBottom(0, 1, DefaultSpecialItem.NEXT_PAGE, clickContext -> {
			openShopEditor(player, shop, fromPage, shopMode, shopPage + 1);
		});
		menu.setItemAndClickHandlerBottom(0, 2, getDefaultModeItem(shop.getDefaultShopMode()), clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				shop.setDefaultShopMode(shop.getDefaultShopMode().getPrevious());
				openShopEditor(player, shop, fromPage, shopMode.getPrevious(), shopPage);
			} else if (clickContext.getAction().isLeftClick()) {
				shop.setDefaultShopMode(shop.getDefaultShopMode().getNext());
				openShopEditor(player, shop, fromPage, shopMode.getNext(), shopPage);
			}
		});
		menu.setItemAndClickHandlerBottom(0, 7, ItemStackUtils.createCustomHead(ItemStackUtils.HEAD_URL_LETTER_T,
				Message.MANAGER_GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME, Message.MANAGER_GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE), clickContext -> {
			//TODO template entries adden
		});
		menu.openInventory(player);
	}

	public void openShopTagsMenu(Player player, Shop shop, int fromPage, int page) {
		PagedChestMenu menu = new PagedChestMenu(Message.MANAGER_GUI_SHOP_TAGS_TITLE.getTranslation(
				Template.of("shop", shop.getName())), 3, null, null, backContext -> {
			openShopMenu(player, shop, fromPage);
		});
		menu.setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD,
				Message.MANAGER_GUI_SHOP_TAGS_NEW_TAG_NAME, Message.MANAGER_GUI_SHOP_TAGS_NEW_TAG_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("tag-me")
					.title(Message.MANAGER_GUI_SHOP_TAGS_NEW_TAG_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openShopTagsMenu(p, shop, fromPage, page), 1L))
					.onComplete((p, s) -> {
						shop.addTag(s);
						openShopTagsMenu(player, shop, fromPage, menu.getPageCount() - 1);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		for (String tag : shop.getTags()) {
			menu.addMenuEntry(ItemStackUtils.createItemStack(Material.NAME_TAG, Component.text(tag, NamedTextColor.WHITE), new ArrayList<>()), clickContext -> {
				if (clickContext.getAction().isRightClick()) {
					if (ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion()) {
						ConfirmMenu confirmMenu = new ConfirmMenu(Message.GENERAL_GUI_REMOVE_TAG.getTranslation(Template.of("tag", tag)));
						confirmMenu.setDenyHandler(c -> openShopTagsMenu(player, shop, fromPage, menu.getCurrentPage()));
						confirmMenu.setCloseHandler(c -> openShopTagsMenu(player, shop, fromPage, menu.getCurrentPage()));
						confirmMenu.setAcceptHandler(c -> {
							shop.removeTag(tag);
							openShopTagsMenu(player, shop, fromPage, menu.getCurrentPage());
						});
						confirmMenu.openInventory(player);
					} else {
						shop.removeTag(tag);
						openShopTagsMenu(player, shop, fromPage, menu.getCurrentPage());
					}
				}
			});
		}
		menu.openInventory(player, page);
	}

	public void openShopLimitsMenu(Player player, Shop shop, int page) {
		player.sendMessage("openShopLimitsMenu");
	}

	public void openShopDiscountsMenu(Player player, Shop shop, int page) {
		player.sendMessage("openShopDiscountsMenu");
	}

	public void openLimitsMenu(Player player) {
		PagedChestMenu chestMenu = new PagedChestMenu(Message.MANAGER_GUI_LIMITS.getTranslation(), 4, null,
				closeContext -> {

				}, backContext -> {
			openBaseMenu(player);
		});

		//List<Limit> limitList = LimitsHandler.getInstance().getLimits();

		chestMenu.setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD,
				Message.MANAGER_GUI_LIMITS_NEW_NAME, Message.MANAGER_GUI_LIMITS_NEW_LORE), clickContext -> {
			//TODO Anvilgui für benennung
		});
		chestMenu.openInventory(player);
	}

	public void openDiscountsMenu(Player player, int page) {
		PagedChestMenu chestMenu = new PagedChestMenu(Message.MANAGER_GUI_DISCOUNTS.getTranslation(), 4, null, null, backContext -> {
			openBaseMenu(player);
		});

		List<Discount> discounts = DiscountHandler.getInstance().getDiscounts();
		for (Discount discount : discounts) {
			chestMenu.addMenuEntry(ItemStackUtils.createDiscountItemStack(discount), clickContext -> {
				if (discount.getEditor() != null) {
					CustomerHandler.getInstance().getCustomer(player).sendMessage(Message.MANAGER_GUI_DISCOUNTS_ALREADY_EDITED.getTranslation(
							Template.of("player", discount.getEditor().getName())));
					return;
				}
				if (clickContext.getAction().equals(delete)) {
					ConfirmMenu confirmMenu = new ConfirmMenu(Message.MANAGER_GUI_DISCOUNTS_DELETE_CONFIRM
							.getTranslation(Template.of("discount", discount.getName())), backContext -> openShopsMenu(player, page));
					confirmMenu.setAcceptHandler(clickContext1 -> {
						DiscountHandler.getInstance().deleteDiscount(discount);
						openDiscountsMenu(player, 0);
					});
					confirmMenu.setDenyHandler(clickContext1 -> {
						openDiscountsMenu(player, page);
					});
					confirmMenu.openInventory(player);
				} else {
					openDiscountMenu(player, discount, page);
				}
			});
		}
		chestMenu.setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD,
				Message.MANAGER_GUI_DISCOUNTS_NEW_NAME, Message.MANAGER_GUI_DISCOUNTS_NEW_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("name")
					.title(Message.MANAGER_GUI_DISCOUNTS_NEW_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openDiscountsMenu(player, page), 1L))
					.onComplete((p, s) -> {
						DiscountHandler.getInstance().createDiscount(s, LocalDateTime.now().plus(1, ChronoUnit.DAYS),
								Duration.of(3, ChronoUnit.DAYS), 10);
						openDiscountsMenu(player, page);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		chestMenu.openInventory(player);
	}

	public void openDiscountMenu(Player player, Discount discount, int page) {
		discount.setEditor(player);
		ChestMenu chestMenu = new ChestMenu(Message.MANAGER_GUI_DISCOUNT, 3);
		chestMenu.setBackHandlerAction(backContext -> openDiscountsMenu(player, page));
		chestMenu.setCloseHandler(closeContext -> discount.setEditor(null));

		//Set name
		chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
				Message.MANAGER_GUI_DISCOUNT_SET_NAME_NAME, Message.MANAGER_GUI_DISCOUNT_SET_NAME_LORE), clickContext -> {
			//TODO anvil menü öffnen
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.MANAGER_GUI_DISCOUNT_SET_PERMISSION_NAME.getTranslation(), Message.MANAGER_GUI_DISCOUNT_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", discount.getPermission() == null ? "X" : discount.getPermission())
				)), clickContext -> {
			//TODO anvil menü öffnen
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.MANAGER_GUI_DISCOUNT_SET_TAGS_NAME, Message.MANAGER_GUI_DISCOUNT_SET_TAGS_LORE),
				clickContext -> openDisountTagsMenu(player, discount, 0));

		//TODO setstart
		//TODO setduration
		//TODO setrepeatingduration
		//TODO setpercent

		chestMenu.openInventory(player);
	}

	public void openDisountTagsMenu(Player player, Discount discount, int page) {
		player.sendMessage("openDisountTagsMenu");
	}
}
