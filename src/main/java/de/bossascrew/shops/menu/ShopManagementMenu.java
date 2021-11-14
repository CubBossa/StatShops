package de.bossascrew.shops.menu;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Config;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.CustomerHandler;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.handler.ShopHandler;
import de.bossascrew.shops.handler.TranslationHandler;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.web.WebSessionUtils;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
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
					ConfirmMenu confirmMenu = new ConfirmMenu("Are you sure to delete?", backContext -> openShopMenu(player, shop, page));//TODO translation
					confirmMenu.setAcceptHandler(clickContext1 -> {
						ShopHandler.getInstance().deleteShop(shop);
						openShopsMenu(player, 0);
					});
					confirmMenu.setDenyHandler(clickContext1 -> {
						openShopsMenu(player, page);
					});
					confirmMenu.openInventory(player);
				} else {
					openShopMenu(player, shop, page);
				}
			});
		}
		chestMenu.setNavigationEntry(7, DefaultSpecialItem.MANAGER_SHOPS_NEW.createSpecialItem(), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("Shopname")
					.title("asd") //TODO translation
					.onClose(p -> openShopsMenu(p, page)) //TODO man kann items aus diesem inv nehmen?
					.onComplete((p, s) -> {
						ShopHandler.getInstance().createShop(s);
						openShopsMenu(player, page);
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
			//TODO anvil menü öffnen
		});
		//Set permissions
		chestMenu.setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.MANAGER_GUI_SHOP_SET_PERMISSION_NAME.getTranslation(), Message.MANAGER_GUI_SHOP_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", shop.getPermission() == null ? "X" : shop.getPermission())
				)), clickContext -> {
			//TODO anvil menü öffnen
		});
		//Open Tags menu
		chestMenu.setItemAndClickHandler(0, 4, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.MANAGER_GUI_SHOP_SET_TAGS_NAME, Message.MANAGER_GUI_SHOP_SET_TAGS_LORE),
				clickContext -> openShopTagsMenu(player, shop, 0));
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
			ConfirmMenu confirmMenu = new ConfirmMenu("Are you sure?", backContext -> openShopMenu(player, shop, fromPage));//TODO translation
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

		chestMenu.setItemAndClickHandler(1, 3, ItemStackUtils.createItemStack(Material.CHEST,
				Message.MANAGER_GUI_SHOP_SET_CONTENT_NAME, Message.MANAGER_GUI_SHOP_SET_CONTENT_LORE), clickContext -> {
			player.sendMessage("openShopEditor");
		});

		//Shopmode switch button
		if (shop.getDefaultShopMode() != null) {

			chestMenu.setItem(19, getDefaultModeItem(shop.getDefaultShopMode()));
			chestMenu.setClickHandler(19, clickContext -> {
				if (clickContext.getAction().isRightClick()) {
					shop.setDefaultShopMode(shop.getDefaultShopMode().getNext());
				} else if (clickContext.getAction().isLeftClick()) {
					shop.setDefaultShopMode(shop.getDefaultShopMode().getPrevious());
				}
				chestMenu.setItem(19, getDefaultModeItem(shop.getDefaultShopMode()));
				chestMenu.refresh(19);
			});
		}
		chestMenu.setItemAndClickHandler(20, getDefaultPageItem(shop, shop.getDefaultShopPage()), clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				shop.setDefaultShopPage((shop.getDefaultShopPage() - 1) % shop.getPageCount());
			} else if (clickContext.getAction().isLeftClick()) {
				shop.setDefaultShopPage((shop.getDefaultShopPage() + 1) % shop.getPageCount());
			}
			chestMenu.setItem(20, getDefaultPageItem(shop, shop.getDefaultShopPage()));
			chestMenu.refresh(20);
		});


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

	private ItemStack getDefaultModeItem(ShopMode shopMode) {
		ItemStack modeItem = shopMode.getDisplayItem();
		List<Component> lore = new ArrayList<>();
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getPrevious().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getDisplayName())));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getPrevious().getDisplayName().color(NamedTextColor.GRAY))));
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

	public void openShopTagsMenu(Player player, Shop shop, int page) {
		player.sendMessage("openShopTagsMenu");
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
				openDiscountMenu(player, discount, page);
			});
		}
		chestMenu.setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD,
				Message.MANAGER_GUI_DISCOUNTS_NEW_NAME, Message.MANAGER_GUI_DISCOUNTS_NEW_LORE), clickContext -> {
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("Discountname")
					.title("asd") //TODO translation
					.onClose(p -> openDiscountsMenu(p, page)) //TODO man kann items aus diesem inv nehmen?
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
				Message.MANAGER_GUI_SHOP_SET_NAME_NAME, Message.MANAGER_GUI_SHOP_SET_NAME_LORE), clickContext -> {
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
