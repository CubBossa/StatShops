package de.bossascrew.shops.menu;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Config;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.handler.LimitsHandler;
import de.bossascrew.shops.handler.ShopHandler;
import de.bossascrew.shops.handler.TranslationHandler;
import de.bossascrew.shops.shop.*;
import de.bossascrew.shops.util.ItemStackUtils;
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
		chestMenu.fillMenu();

		chestMenu.setItemAndClickHandler(1, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_SHOP,
				Message.MANAGER_GUI_MAIN_SHOPS_NAME, Message.MANAGER_GUI_MAIN_SHOPS_LORE), clickContext -> openShopsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
				Message.MANAGER_GUI_MAIN_LIMITS_NAME, Message.MANAGER_GUI_MAIN_LIMITS_LORE), clickContext -> openLimitsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 3, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
				Message.MANAGER_GUI_MAIN_DISCOUNTS_NAME, Message.MANAGER_GUI_MAIN_DISCOUNTS_LORE), clickContext -> openDiscountsMenu(player, 0));
		chestMenu.setItemAndClickHandler(1, 6, ItemStackUtils.createItemStack(Material.BOOK,
				Message.MANAGER_GUI_MAIN_LANGUAGE_NAME.getTranslation(), Message.MANAGER_GUI_MAIN_LANGUAGE_LORE.getTranslations(Template.of("file",
						ShopPlugin.getInstance().getShopsConfig().getLanguage() + ".yml"))), clickContext -> {
			if (clickContext.getAction().equals(ClickType.RIGHT)) {
				long ms = System.currentTimeMillis();
				ShopPlugin.getInstance().getShopsConfig().reloadLanguage();
				TranslationHandler.getInstance().loadLanguage(ShopPlugin.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
					if (success) {
						openBaseMenu(player);
						Customer.wrap(player).sendMessage(Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getTranslation(Template.of("ms", System.currentTimeMillis() - ms + "")));
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
					customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getTranslation(Template.of("link", "https://127.0.0.1:8080/" + paste.getId())));
				});
			}
		});
		chestMenu.setItemAndClickHandler(1, 7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_WEBINTERFACE,
				Message.MANAGER_GUI_MAIN_WEBINTERFACE_NAME, Message.MANAGER_GUI_MAIN_WEBINTERFACE_LORE), clickContext -> {
			player.closeInventory();
			Customer customer = Customer.wrap(player);
			customer.sendMessage(Message.GENERAL_WEBINTERFACE_LOADING);

			ShopPlugin.getInstance().runAsync(() -> {

				Paste paste = WebSessionUtils.generateWebSession();
				if (paste == null) {
					customer.sendMessage(Message.GENERAL_WEBINTERFACE_ERROR);
					return;
				}
				customer.sendMessage(Message.GENERAL_WEBINTERFACE_LINK.getTranslation(Template.of("link", "https://127.0.0.1:8080/" + paste.getId())));
			});

		});
		chestMenu.openInventory(player);
	}

	public void openShopsMenu(Player player, int page) {
		ListManagerMenu<Shop> menu = new ListManagerMenu<>(4, ShopHandler.getInstance(), ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion(),
				Message.MANAGER_GUI_SHOPS_TITLE, Message.MANAGER_GUI_SHOPS_ALREADY_EDITED, Message.MANAGER_GUI_SHOPS_NEW_NAME,
				Message.MANAGER_GUI_SHOPS_NEW_LORE, Message.MANAGER_GUI_SHOPS_DELETE_CONFIRM, Message.MANAGER_GUI_SHOPS_NEW_TITLE, backContext -> openBaseMenu(player));
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
				Message.MANAGER_GUI_SHOP_SET_NAME_NAME, Message.MANAGER_GUI_SHOP_SET_NAME_LORE), clickContext -> {
			if (clickContext.getPlayer().getItemOnCursor().getType() != Material.AIR) {
				shop.setDisplayMaterial(clickContext.getPlayer().getItemOnCursor().getType());
				ShopPlugin.getInstance().getDatabase().saveShop(shop);
				clickContext.setItemStack(ItemStackUtils.createItemStack(shop.getDisplayMaterial(), Message.MANAGER_GUI_SHOP_SET_NAME_NAME, Message.MANAGER_GUI_SHOP_SET_NAME_LORE));
				return;
			}
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
					.text("shops.shop." + shop.getNamePlain().toLowerCase() + ".")
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
				clickContext -> new TagsEditorMenu(
						shop, Message.MANAGER_GUI_TAGS_TITLE.getTranslation(Template.of("name", shop.getName())),
						Message.MANAGER_GUI_TAGS_NEW_TAG_TITLE, Message.MANAGER_GUI_TAGS_NEW_TAG_NAME, Message.MANAGER_GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openShopMenu(player, shop, fromPage)).openInventory(player));

		//Open Limits menu
		chestMenu.setItemAndClickHandler(0, 5, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
						Message.MANAGER_GUI_SHOP_SET_LIMITS_NAME, Message.MANAGER_GUI_SHOP_SET_LIMITS_LORE),
				clickContext -> openShopLimitsMenu(player, shop, 0));
		//Open Discounts menu
		chestMenu.setItemAndClickHandler(0, 6, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
						Message.MANAGER_GUI_SHOP_SET_DISCOUNTS_NAME, Message.MANAGER_GUI_SHOP_SET_DISCOUNTS_LORE),
				clickContext -> openShopDiscountsMenu(player, shop, 0));

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
		new ShopEditor(shop, backContext -> openShopMenu(player, shop, fromPage)).openInventory(player, shopMode, shopPage);
	}

	public void openShopLimitsMenu(Player player, Shop shop, int page) {
		player.sendMessage("openShopLimitsMenu");
	}

	public void openShopDiscountsMenu(Player player, Shop shop, int page) {
		player.sendMessage("openShopDiscountsMenu");
	}

	public void openLimitsMenu(Player player, int page) {
		ListManagerMenu<Limit> menu = new ListManagerMenu<>(4, LimitsHandler.getInstance(), ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion(),
				Message.MANAGER_GUI_LIMITS, Message.MANAGER_GUI_LIMITS_ALREADY_EDITED, Message.MANAGER_GUI_LIMITS_NEW_NAME, Message.MANAGER_GUI_LIMITS_NEW_LORE,
				Message.MANAGER_GUI_LIMITS_DELETE_CONFIRM, Message.MANAGER_GUI_LIMITS_NEW_TITLE, backContext -> openBaseMenu(player));
		menu.setLeftClickHandler(targetContext -> openLimitMenu(player, targetContext.getTarget(), menu.currentPage));
		menu.openInventory(player);
	}

	public void openLimitMenu(Player player, Limit limit, int fromPage) {
		limit.setEditor(player);
		ChestMenu chestMenu = new ChestMenu(limit.getName(), 3);
		chestMenu.setBackHandlerAction(backContext -> openLimitsMenu(player, fromPage));
		//Set name
		chestMenu.setItemAndClickHandler(0, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Message.MANAGER_GUI_LIMIT_SET_NAME_NAME, Message.MANAGER_GUI_LIMIT_SET_NAME_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text(limit.getNameFormat())
					.title(Message.MANAGER_GUI_LIMIT_SET_NAME_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openLimitMenu(p, limit, fromPage), 1L))
					.onComplete((p, s) -> {
						limit.setNameFormat(s);
						openLimitMenu(player, limit, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.MANAGER_GUI_LIMIT_SET_PERMISSION_NAME.getTranslation(), Message.MANAGER_GUI_LIMIT_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", limit.getPermission() == null ? "X" : limit.getPermission())
				)), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("shops.limit." + limit.getNamePlain().toLowerCase() + ".")
					.title(Message.MANAGER_GUI_SHOP_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openLimitMenu(p, limit, fromPage), 1L))
					.onComplete((p, s) -> {
						limit.setPermission(s);
						openLimitMenu(player, limit, fromPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.MANAGER_GUI_LIMIT_SET_TAGS_NAME, Message.MANAGER_GUI_LIMIT_SET_TAGS_LORE),
				clickContext -> new TagsEditorMenu(
						limit, Message.MANAGER_GUI_TAGS_TITLE.getTranslation(Template.of("name", limit.getName())),
						Message.MANAGER_GUI_TAGS_NEW_TAG_TITLE, Message.MANAGER_GUI_TAGS_NEW_TAG_NAME, Message.MANAGER_GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openLimitMenu(player, limit, fromPage)).openInventory(player));


		chestMenu.openInventory(player);
	}

	public void openDiscountsMenu(Player player, int page) {
		ListManagerMenu<Discount> menu = new ListManagerMenu<>(4, DiscountHandler.getInstance(), ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion(),
				Message.MANAGER_GUI_DISCOUNTS, Message.MANAGER_GUI_DISCOUNTS_ALREADY_EDITED, Message.MANAGER_GUI_DISCOUNTS_NEW_NAME,
				Message.MANAGER_GUI_DISCOUNTS_NEW_LORE, Message.MANAGER_GUI_DISCOUNTS_DELETE_CONFIRM, Message.MANAGER_GUI_DISCOUNTS_NEW_TITLE, backContext -> openBaseMenu(player));
		menu.setLeftClickHandler(targetContext -> openDiscountMenu(player, targetContext.getTarget(), menu.getCurrentPage()));
		menu.openInventory(player, page);
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
				clickContext -> new TagsEditorMenu(
						discount, Message.MANAGER_GUI_TAGS_TITLE.getTranslation(Template.of("name", discount.getName())),
						Message.MANAGER_GUI_TAGS_NEW_TAG_TITLE, Message.MANAGER_GUI_TAGS_NEW_TAG_NAME, Message.MANAGER_GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openDiscountMenu(player, discount, page)).openInventory(player));

		//TODO setstart
		//TODO setduration
		//TODO setrepeatingduration
		//TODO setpercent

		chestMenu.openInventory(player);
	}
}
