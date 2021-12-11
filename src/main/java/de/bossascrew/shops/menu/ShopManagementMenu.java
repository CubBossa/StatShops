package de.bossascrew.shops.menu;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.*;
import de.bossascrew.shops.shop.*;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.util.TagUtils;
import de.bossascrew.shops.web.WebSessionUtils;
import de.bossascrew.shops.web.pasting.Paste;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

public class ShopManagementMenu {

	public void openBaseMenu(Player player) {
		ChestMenu chestMenu = new ChestMenu(Message.GUI_MAIN_TITLE, 3);
		chestMenu.fillMenu();

		chestMenu.setItemAndClickHandler(1, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_SHOP,
				Message.GUI_MAIN_SHOPS_NAME, Message.GUI_MAIN_SHOPS_LORE), clickContext -> openShopsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
				Message.GUI_MAIN_LIMITS_NAME, Message.GUI_MAIN_LIMITS_LORE), clickContext -> openLimitsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 3, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
				Message.GUI_MAIN_DISCOUNTS_NAME, Message.GUI_MAIN_DISCOUNTS_LORE), clickContext -> openDiscountsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 6, ItemStackUtils.createItemStack(Material.BOOK,
				Message.GUI_MAIN_LANGUAGE_NAME.getTranslation(), Message.GUI_MAIN_LANGUAGE_LORE.getTranslations(Template.of("file",
						ShopPlugin.getInstance().getShopsConfig().getLanguage() + ".yml"))), clickContext -> {
			if (clickContext.getAction().equals(ClickType.RIGHT)) {
				long ms = System.currentTimeMillis();
				TranslationHandler.getInstance().loadLanguage(ShopPlugin.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
					if (success) {
						Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getKey(),
								Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getTranslation(Template.of("ms", System.currentTimeMillis() - ms + "")), 0);
						openBaseMenu(player);
						return;
					}
					Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOAD_ERROR);
					chestMenu.setItem(1, 6, DefaultSpecialItem.ERROR.createSpecialItem());
				});
			} else if (clickContext.getAction().equals(ClickType.LEFT)) {
				player.closeInventory();
				Customer customer = Customer.wrap(player);
				customer.sendMessage(Message.GENERAL_WEBINTERFACE_LOADING);

				ShopPlugin.getInstance().runAsync(() -> {

					Paste paste = WebSessionUtils.generateWebSession();
					if (paste == null) {
						customer.sendMessage(Message.GENERAL_WEBINTERFACE_ERROR); //TODO natürlich das language webinterface
						return;
					}
					customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getKey(), Message.GENERAL_WEBINTERFACE_LINK.getTranslation(Template.of("link", "https://127.0.0.1:8080/" + paste.getId())));
				});
			}
		});
		chestMenu.setItemAndClickHandler(1, 7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_WEBINTERFACE,
				Message.GUI_MAIN_WEBINTERFACE_NAME, Message.GUI_MAIN_WEBINTERFACE_LORE), clickContext -> {
			player.closeInventory();
			Customer customer = Customer.wrap(player);
			customer.sendMessage(Message.GENERAL_WEBINTERFACE_LOADING);

			ShopPlugin.getInstance().runAsync(() -> {

				Paste paste = WebSessionUtils.generateWebSession();
				if (paste == null) {
					customer.sendMessage(Message.GENERAL_WEBINTERFACE_ERROR);
					return;
				}
				customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getKey(), Message.GENERAL_WEBINTERFACE_LINK.getTranslation(Template.of("link", "https://127.0.0.1:8080/" + paste.getId())));
			});

		});
		chestMenu.openInventory(player);
	}

	public void openShopsMenu(Player player, int page) {
		ListManagerMenu<Shop> menu = new ListManagerMenu<>(4, ShopHandler.getInstance(), ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion(),
				Message.GUI_SHOPS_TITLE, Message.GUI_SHOPS_ALREADY_EDITED, Message.GUI_SHOPS_NEW_NAME,
				Message.GUI_SHOPS_NEW_LORE, Message.GUI_SHOPS_DELETE_CONFIRM, Message.GUI_SHOPS_NEW_TITLE, backContext -> openBaseMenu(player));
		menu.setLeftClickHandler(targetContext -> openShopMenu(player, targetContext.getTarget(), menu.getCurrentPage()));
		menu.openInventory(player, page);
	}

	public void openShopMenu(Player player, Shop shop, int fromPage) {

		//deactivate shop so we can change stuff without worrying about players messing things up
		//the shop will activate itself automatically when using the back button. Otherwise it needs to be reactivated manually
		shop.setEnabled(false);
		shop.setEditor(player);

		ChestMenu chestMenu = new ChestMenu(shop.getName(), 3);
		chestMenu.fillMenu();
		//Set name
		chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(shop.getDisplayMaterial() == null ?
						ItemStackUtils.MATERIAL_SHOP : shop.getDisplayMaterial(),
				Message.GUI_SHOP_SET_NAME_NAME, Message.GUI_SHOP_SET_NAME_LORE), clickContext -> {
			if (clickContext.getPlayer().getItemOnCursor().getType() != Material.AIR) {
				shop.setDisplayMaterial(clickContext.getPlayer().getItemOnCursor().getType());
				ShopPlugin.getInstance().getDatabase().saveShop(shop);
				clickContext.setItemStack(ItemStackUtils.createItemStack(shop.getDisplayMaterial(), Message.GUI_SHOP_SET_NAME_NAME, Message.GUI_SHOP_SET_NAME_LORE));
				return;
			}
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text(shop.getNameFormat())
					.title(Message.GUI_SHOP_SET_NAME_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openShopMenu(p, shop, fromPage), 1L))
					.onComplete((p, s) -> {
						shop.setNameFormat(s);
						openShopMenu(player, shop, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.GUI_SHOP_SET_PERMISSION_NAME.getTranslation(), Message.GUI_SHOP_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", shop.getPermission() == null ? "X" : shop.getPermission())
				)), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("shops.shop." + shop.getNamePlain().toLowerCase() + ".")
					.title(Message.GUI_SHOP_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openShopMenu(p, shop, fromPage), 1L))
					.onComplete((p, s) -> {
						shop.setPermission(s);
						openShopMenu(player, shop, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.GUI_SHOP_SET_TAGS_NAME, Message.GUI_SHOP_SET_TAGS_LORE),
				clickContext -> new TagsEditorMenu(
						shop, Message.GUI_TAGS_TITLE.getTranslation(Template.of("name", shop.getName())),
						Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openShopMenu(player, shop, fromPage)).openInventory(player));

		//Open Limits menu
		chestMenu.setItemAndClickHandler(0, 5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
						Message.GUI_SHOP_SET_LIMITS_NAME, Message.GUI_SHOP_SET_LIMITS_LORE),
				clickContext -> openShopLimitsMenu(player, shop, fromPage, 0));
		//Open Discounts menu
		chestMenu.setItemAndClickHandler(0, 6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
						Message.GUI_SHOP_SET_DISCOUNTS_NAME, Message.GUI_SHOP_SET_DISCOUNTS_LORE),
				clickContext -> openShopDiscountsMenu(player, shop, fromPage, 0));

		//Open Templates menu
		chestMenu.setItemAndClickHandler(0, 7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
						Message.GUI_SHOP_SET_TEMPLATE_NAME.getTranslation(), Message.GUI_SHOP_SET_TEMPLATE_LORE.getTranslations(Template.of("template",
								shop.getDefaultTemplate() == null ? Component.text("none") : shop.getDefaultTemplate().getName()))),
				clickContext -> openDefaultTemplateMenu(player, shop, fromPage, 0));

		//Assign Shop to NPC
		if (ShopPlugin.getInstance().isCitizensInstalled()) {

			chestMenu.setItemAndClickHandler(0, 8, ItemStackUtils.createItemStack(Material.PLAYER_HEAD,
					Message.GUI_SHOP_SET_NPC_NAME, Message.GUI_SHOP_SET_NPC_LORE), clickContext -> {
				ShopPlugin.getInstance().getCitizensHook().addAssigningPlayer(player, shop);
				player.closeInventory();
				Customer.wrap(player).sendMessage(Message.CITIZENS_CLICK_TO_ASSIGN);
			});
		}

		chestMenu.setItemAndClickHandler(1, 1, ItemStackUtils.createItemStack(Material.SMITHING_TABLE,
				Message.GUI_SHOP_SET_CONTENT_NAME, Message.GUI_SHOP_SET_CONTENT_LORE), clickContext -> {
			shop.openEditorMenu(player, backContext -> openShopMenu(player, shop, fromPage));
		});
		chestMenu.setItemAndClickHandler(1, 2, ItemStackUtils.createItemStack(Material.CHEST,
				Message.GUI_SHOP_SET_PREVIEW_NAME, Message.GUI_SHOP_SET_PREVIEW_LORE), clickContext -> {
			shop.open(Customer.wrap(player), backContext -> openShopMenu(player, shop, fromPage));
		});

		if (shop instanceof ModedShop ms) {
			//Shopmode switch button
			chestMenu.setItem(19, getDefaultModeItem(ms.getDefaultShopMode()));
			chestMenu.setClickHandler(19, clickContext -> {
				if (clickContext.getAction().isRightClick()) {
					ms.setDefaultShopMode(ms.getDefaultShopMode().getPrevious());
				} else if (clickContext.getAction().isLeftClick()) {
					ms.setDefaultShopMode(ms.getDefaultShopMode().getNext());
				}
				chestMenu.setItem(19, getDefaultModeItem(ms.getDefaultShopMode()));
				chestMenu.refresh(19);
			});

			//Set mode remembered
			ItemStack rememberMode = getButton(ms.isModeRemembered(),
					Message.GUI_SHOP_SET_REMEMBER_MODE_NAME, Message.GUI_SHOP_SET_REMEMBER_MODE_LORE);
			chestMenu.setItemAndClickHandler(24, rememberMode, clickContext -> {
				ms.setModeRemembered(!ms.isModeRemembered());
				chestMenu.setItem(24, getButton(ms.isModeRemembered(),
						Message.GUI_SHOP_SET_REMEMBER_MODE_NAME, Message.GUI_SHOP_SET_REMEMBER_MODE_LORE));
				chestMenu.refresh(24);
			});
		}

		if (shop instanceof PaginatedShop ps) {
			chestMenu.setItemAndClickHandler(20, getDefaultPageItem(ps, ps.getDefaultShopPage()), clickContext -> {
				if (clickContext.getAction().isRightClick()) {
					ps.setDefaultShopPage((ps.getDefaultShopPage() - 1) % ps.getPageCount());
				} else if (clickContext.getAction().isLeftClick()) {
					ps.setDefaultShopPage((ps.getDefaultShopPage() + 1) % ps.getPageCount());
				}
				chestMenu.setItem(20, getDefaultPageItem(ps, ps.getDefaultShopPage()));
				chestMenu.refresh(20);
			});

			//Set page remembered
			ItemStack rememberPage = getButton(ps.isPageRemembered(),
					Message.GUI_SHOP_SET_REMEMBER_PAGE_NAME, Message.GUI_SHOP_SET_REMEMBER_PAGE_LORE);
			chestMenu.setItemAndClickHandler(23, rememberPage, clickContext -> {
				ps.setPageRemembered(!ps.isPageRemembered());
				chestMenu.setItem(23, getButton(ps.isPageRemembered(),
						Message.GUI_SHOP_SET_REMEMBER_PAGE_NAME, Message.GUI_SHOP_SET_REMEMBER_PAGE_LORE));
				chestMenu.refresh(23);
			});
		}
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
				Message.GUI_SHOP_SET_ENABLED_NAME, Message.GUI_SHOP_SET_ENABLED_LORE);
		chestMenu.setItemAndClickHandler(22, shopEnabled, clickContext -> {
			shop.setEnabled(!shop.isEnabled());
			chestMenu.setItem(22, getButton(shop.isEnabled(),
					Message.GUI_SHOP_SET_ENABLED_NAME, Message.GUI_SHOP_SET_ENABLED_LORE));
			chestMenu.refresh(22);
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
		if (prev <= 0) {
			prev += 6;
		}
		int next = row + 1 % 6;
		if (next <= 0) {
			next += 6;
		}
		if (row == 0) {
			row = 6;
		}
		lore.add(Message.GUI_SHOP_SET_ROWS_LORE.getTranslation(Template.of("rows", "" + prev)));
		lore.add(Message.GUI_SHOP_SET_ROWS_LORE.getTranslation(Template.of("rows", "" + row)));
		lore.add(Message.GUI_SHOP_SET_ROWS_LORE.getTranslation(Template.of("rows", "" + next)));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));

		return ItemStackUtils.createItemStack(Material.RAIL, Message.GUI_SHOP_SET_ROWS_NAME.getTranslation(
				Template.of("rows", "" + row)), lore);
	}

	private ItemStack getDefaultModeItem(ShopMode shopMode) {
		ItemStack modeItem = shopMode.getDisplayItem();
		List<Component> lore = new ArrayList<>();
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getPrevious().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getDisplayName())));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getNext().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		ItemStack item = ItemStackUtils.createItemStack(modeItem.getType(),
				Message.GUI_SHOP_SET_DEFAULT_MODE_NAME.getTranslation(Template.of("name", modeItem.getItemMeta().getDisplayName())),
				lore);
		return item;
	}

	private ItemStack getDefaultPageItem(PaginatedShop shop, int page) {
		int pageCount = shop.getPageCount();
		List<Component> lore = new ArrayList<>();
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(Template.of("page", "" + ((page - 1) % pageCount))));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(Template.of("page", "" + page)));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_PAGE_LORE.getTranslation(Template.of("page", "" + ((page + 1) % pageCount))));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		return ItemStackUtils.createItemStack(Material.BOOK,
				Message.GUI_SHOP_SET_DEFAULT_PAGE_NAME.getTranslation(Template.of("page", "" + page)), lore);
	}

	private ItemStack getButton(boolean val, Message name, Message lore) {
		return ItemStackUtils.createItemStack(val ? Material.LIME_DYE : Material.GRAY_DYE,
				name.getTranslation(Template.of("value", val + "")),
				lore.getTranslations(Template.of("value", val + "")));
	}

	public void openShopLimitsMenu(Player player, Shop shop, int fromPage, int page) {
		ListMenu<Limit> listMenu = new ListMenu<>(3, LimitsHandler.getInstance(), Message.GUI_SHOP_LIMITS_TITLE, backContext -> openShopMenu(player, shop, fromPage));
		listMenu.setNavigationEntry(4, ItemStackUtils.createItemStack(Material.PAPER, Message.GUI_SHOP_LIMITS_INFO_NAME, Message.GUI_SHOP_LIMITS_INFO_LORE), clickContext -> {
		});
		listMenu.setGlowPredicate(limit -> TagUtils.hasCommonTags(shop, limit));
		listMenu.setClickHandler(targetContext -> {
			Limit limit = targetContext.getTarget();
			if (targetContext.getAction().isRightClick()) {
				limit.removeTag(shop.getUUID().toString());
			} else if (targetContext.getAction().isLeftClick()) {
				limit.addTag(shop.getUUID().toString());
			}
			listMenu.openInventory(player, listMenu.getCurrentPage());
		});
		listMenu.openInventory(player, page);
	}

	public void openShopDiscountsMenu(Player player, Shop shop, int fromPage, int page) {
		ListMenu<Discount> listMenu = new ListMenu<>(3, DiscountHandler.getInstance(), Message.GUI_SHOP_DISCOUNTS_TITLE, backContext -> openShopMenu(player, shop, fromPage));
		listMenu.setNavigationEntry(4, ItemStackUtils.createItemStack(Material.PAPER, Message.GUI_SHOP_DISCOUNTS_INFO_NAME, Message.GUI_SHOP_DISCOUNTS_INFO_LORE), clickContext -> {
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

	public void openDefaultTemplateMenu(Player player, Shop shop, int fromPage, int page) {
		ListMenu<EntryTemplate> listMenu = new ListMenu<>(3, TemplateHandler.getInstance(), Message.GUI_SHOP_TEMPLATE_TITLE, backContext -> openShopMenu(player, shop, fromPage));
		listMenu.setNavigationEntry(4, ItemStackUtils.createItemStack(Material.PAPER, Message.GUI_SHOP_TEMPLATE_INFO_NAME, Message.GUI_SHOP_TEMPLATE_INFO_LORE), clickContext -> {
		});
		listMenu.setGlowPredicate(template -> shop.getDefaultTemplate() != null && template.equals(shop.getDefaultTemplate()));
		listMenu.setClickHandler(targetContext -> {
			EntryTemplate template = targetContext.getTarget();
			if (shop.getDefaultTemplate() != null && template.equals(shop.getDefaultTemplate())) {
				shop.setDefaultTemplate(null);
			} else {
				shop.setDefaultTemplate(template);
			}
			openDefaultTemplateMenu(player, shop, fromPage, listMenu.getCurrentPage());
		});
		listMenu.openInventory(player, page);
	}

	public void openLimitsMenu(Player player, int page) {
		ListManagerMenu<Limit> menu = new ListManagerMenu<>(4, LimitsHandler.getInstance(), ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion(),
				Message.GUI_LIMITS, Message.GUI_LIMITS_ALREADY_EDITED, Message.GUI_LIMITS_NEW_NAME, Message.GUI_LIMITS_NEW_LORE,
				Message.GUI_LIMITS_DELETE_CONFIRM, Message.GUI_LIMITS_NEW_TITLE, backContext -> openBaseMenu(player));
		menu.setLeftClickHandler(targetContext -> openLimitMenu(player, targetContext.getTarget(), menu.currentPage));
		menu.openInventory(player, page);
	}

	public void openLimitMenu(Player player, Limit limit, int fromPage) {
		limit.setEditor(player);
		ChestMenu chestMenu = new ChestMenu(limit.getName(), 3);
		chestMenu.setCloseHandler(closeContext -> limit.setEditor(null));
		chestMenu.setBackHandlerAction(backContext -> openLimitsMenu(player, fromPage));
		//Set name
		chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Message.GUI_LIMIT_SET_NAME_NAME, Message.GUI_LIMIT_SET_NAME_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text(limit.getNameFormat())
					.title(Message.GUI_LIMIT_SET_NAME_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openLimitMenu(p, limit, fromPage), 1L))
					.onComplete((p, s) -> {
						limit.setNameFormat(s);
						openLimitMenu(player, limit, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.GUI_LIMIT_SET_PERMISSION_NAME.getTranslation(), Message.GUI_LIMIT_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", limit.getPermission() == null ? "X" : limit.getPermission())
				)), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("shops.limit." + limit.getNamePlain().toLowerCase() + ".")
					.title(Message.GUI_SHOP_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openLimitMenu(p, limit, fromPage), 1L))
					.onComplete((p, s) -> {
						limit.setPermission(s);
						openLimitMenu(player, limit, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.GUI_LIMIT_SET_TAGS_NAME, Message.GUI_LIMIT_SET_TAGS_LORE),
				clickContext -> new TagsEditorMenu(
						limit, Message.GUI_TAGS_TITLE.getTranslation(Template.of("name", limit.getName())),
						Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openLimitMenu(player, limit, fromPage)).openInventory(player));


		chestMenu.openInventory(player);
	}

	public void openDiscountsMenu(Player player, int page) {
		ListManagerMenu<Discount> menu = new ListManagerMenu<>(4, DiscountHandler.getInstance(), ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion(),
				Message.GUI_DISCOUNTS, Message.GUI_DISCOUNTS_ALREADY_EDITED, Message.GUI_DISCOUNTS_NEW_NAME,
				Message.GUI_DISCOUNTS_NEW_LORE, Message.GUI_DISCOUNTS_DELETE_CONFIRM, Message.GUI_DISCOUNTS_NEW_TITLE, backContext -> openBaseMenu(player));
		menu.setLeftClickHandler(targetContext -> openDiscountMenu(player, targetContext.getTarget(), menu.getCurrentPage()));
		menu.openInventory(player, page);
	}

	public void openDiscountMenu(Player player, Discount discount, int fromPage) {
		discount.setEditor(player);
		ChestMenu chestMenu = new ChestMenu(Message.GUI_DISCOUNT, 3);
		chestMenu.setBackHandlerAction(backContext -> openDiscountsMenu(player, fromPage));
		chestMenu.setCloseHandler(closeContext -> discount.setEditor(null));

		//Set name
		chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_DISCOUNT_SET_NAME_NAME, Message.GUI_DISCOUNT_SET_NAME_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text(discount.getNameFormat())
					.title(Message.GUI_DISCOUNT_SET_NAME_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openDiscountMenu(p, discount, fromPage), 1L))
					.onComplete((p, s) -> {
						discount.setNameFormat(s);
						openDiscountMenu(player, discount, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.GUI_DISCOUNT_SET_PERMISSION_NAME.getTranslation(), Message.GUI_DISCOUNT_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", discount.getPermission() == null ? "X" : discount.getPermission())
				)), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("shops.discount." + discount.getNamePlain().toLowerCase() + ".")
					.title(Message.GUI_DISCOUNT_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openDiscountMenu(p, discount, fromPage), 1L))
					.onComplete((p, s) -> {
						discount.setPermission(s);
						openDiscountMenu(player, discount, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.GUI_DISCOUNT_SET_TAGS_NAME, Message.GUI_DISCOUNT_SET_TAGS_LORE),
				clickContext -> new TagsEditorMenu(
						discount, Message.GUI_TAGS_TITLE.getTranslation(Template.of("name", discount.getName())),
						Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openDiscountMenu(player, discount, fromPage)).openInventory(player));

		chestMenu.setItemAndClickHandler(0, 5, ItemStackUtils.createItemStack(Material.CLOCK, Message.GUI_DISCOUNT_SET_START_NAME,
				Message.GUI_DISCOUNT_SET_START_LORE), clickContext -> {

			PagedChestMenu menu = new PagedChestMenu(Message.GUI_DISCOUNT_SET_START_TITLE.getTranslation(), 3, null, null, backContext -> {
				openDiscountMenu(player, discount, fromPage);
			});

			menu.setNavigationEntry(4, ItemStackUtils.createItemStack(Material.PAPER, Message.GUI_DISCOUNT_START_INFO_NAME,
					Message.GUI_DISCOUNT_START_INFO_LORE), cc -> {
			});

			menu.setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD, Message.GUI_DISCOUNT_START_NEW_NAME,
					Message.GUI_DISCOUNT_START_NEW_LORE), cc -> {

				player.closeInventory();
				new AnvilGUI.Builder()
						.plugin(ShopPlugin.getInstance())
						.text("dd.MM.yy, hh:mm")
						.title(Message.GUI_DISCOUNT_START_NEW_TITLE.getLegacyTranslation())
						.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> menu.openInventory(player, menu.getCurrentPage()), 1L))
						.onComplete((p, s) -> {
							//TODO localdate parsen und einfügen.
							return AnvilGUI.Response.close();
						}).open(player);
			});
			for (LocalDateTime date : discount.getStartTimes()) {
				Component dateComp = Component.text(ComponentUtils.formatLocalDateTime(date), NamedTextColor.WHITE);
				menu.addMenuEntry(ItemStackUtils.createItemStack(Material.NAME_TAG, dateComp, new ArrayList<>()), cc -> {
					if (cc.getAction().isRightClick()) {
						if (ShopPlugin.getInstance().getShopsConfig().isConfirmDeletion()) {
							ConfirmMenu confirmMenu = new ConfirmMenu(Message.GUI_DISCOUNT_START_DELETE_CONFIRM.getTranslation(Template.of("date", dateComp)));
							confirmMenu.setDenyHandler(c -> menu.openInventory(player, menu.getCurrentPage()));
							confirmMenu.setCloseHandler(c -> menu.openInventory(player, menu.getCurrentPage()));
							confirmMenu.setAcceptHandler(c -> {
								discount.removeStartTime(date);
								menu.openInventory(c.getPlayer(), menu.getCurrentPage());
							});
							confirmMenu.openInventory(cc.getPlayer());
						} else {
							discount.removeStartTime(date);
							menu.openInventory(cc.getPlayer(), menu.getCurrentPage());
						}
					}
				});
				menu.openInventory(player);
			}
		});
		chestMenu.setItemAndClickHandler(0, 6, ItemStackUtils.createItemStack(Material.COMPASS, Message.GUI_DISCOUNT_SET_DURATION_NAME,
				Message.GUI_DISCOUNT_SET_DURATION_LORE), clickContext -> {
			//TODO setduration compass

		});
		chestMenu.setItemAndClickHandler(0, 7, ItemStackUtils.createItemStack(Material.EMERALD, Message.GUI_DISCOUNT_SET_PERCENT_NAME,
				Message.GUI_DISCOUNT_SET_PERCENT_LORE), clickContext -> {
			//TODO setpercent emerald

		});

		chestMenu.openInventory(player);
	}
}
