package de.bossascrew.shops.menu;

import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.TemplateHandler;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class ShopEditorPageMenu extends BottomTopChestMenu {

	private final ChestMenuShop shop;
	private final ShopMode shopMode;
	private final int shopPage;

	private final ShopEditor shopEditor;
	private final ContextConsumer<BackContext> backHandler;

	@Getter
	@Setter
	private @Nullable ShopEntry selectedEntry = null;

	private boolean itemsMovable = true;

	public ShopEditorPageMenu(ChestMenuShop shop, ShopMode shopMode, int shopPage, ContextConsumer<BackContext> backHandler, ShopEditor shopEditor) {
		super(Message.SHOP_GUI_TITLE.getTranslation(
				Template.of("name", shop.getName()),
				Template.of("page", "" + (shopPage + 1)),
				Template.of("pages", "" + Integer.max(shop.getPageCount(), shopPage + 1)),
				Template.of("mode", shopMode.getDisplayName())), shop.getRows(), 1);
		this.shop = shop;
		this.shopMode = shopMode;
		this.shopPage = shopPage;
		this.shopEditor = shopEditor;
		this.backHandler = backHandler;
	}

	private void prepareMenu() {
		fillMenu(DefaultSpecialItem.EMPTY_LIGHT);

		for (int i = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE; i < (shopPage + 1) * RowedOpenableMenu.LARGEST_INV_SIZE; i++) {
			ShopEntry entry = shop.getEntry(shopMode, i);
			if (entry == null) {
				continue;
			}
			setItem(i - shopPage * RowedOpenableMenu.LARGEST_INV_SIZE, ItemStackUtils.prepareEditorEntryItemStack(entry, false));
		}
		setDefaultClickHandler(ClickType.LEFT, clickContext -> {
			ShopEntry oldEntry = shop.getEntry(shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);

			//TODO zu umständlich gedacht. Stattdessen nbttag setzen wenn item reingelegt
			if (clickContext.getPlayer().getItemOnCursor().getType() == Material.AIR) {
				setSelectedEntry(oldEntry);
				clickContext.setItemStack(ItemStackUtils.prepareEditorEntryItemStack(oldEntry, true));
			} else {
				ItemStack temp = clickContext.getPlayer().getItemOnCursor();
				ShopEntry entry = shop.createEntry(temp, shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
				clickContext.getPlayer().setItemOnCursor(oldEntry == null ? null : oldEntry.getDisplayItem());
				setSelectedEntry(entry);
				clickContext.setItemStack(ItemStackUtils.prepareEditorEntryItemStack(entry, true));
			}
		});
		setDefaultClickHandler(ClickType.RIGHT, clickContext -> {
			shop.deleteEntry(shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
			clickContext.setItemStack(DefaultSpecialItem.EMPTY_LIGHT.createSpecialItem());
			refresh(clickContext.getSlot());
		});
		fillBottom();
		setBackSlotBottom(8);
		setBackHandlerAction(backHandler);
		setItemAndClickHandlerBottom(0, 0, shopPage > 0 ? DefaultSpecialItem.PREV_PAGE : DefaultSpecialItem.PREV_PAGE_OFF, clickContext -> {
			shopEditor.openInventory(clickContext.getPlayer(), shopMode, shopPage > 0 ? shopPage - 1 : shopPage);
		});
		setItemAndClickHandlerBottom(0, 1, DefaultSpecialItem.NEXT_PAGE, clickContext -> {
			shopEditor.openInventory(clickContext.getPlayer(), shopMode, shopPage + 1);
		});
		setItemAndClickHandlerBottom(0, 2, getDefaultModeItem(shop.getDefaultShopMode()), clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				shop.setDefaultShopMode(shop.getDefaultShopMode().getPrevious());
				shopEditor.openInventory(clickContext.getPlayer(), shopMode.getPrevious(), shopPage);
			} else if (clickContext.getAction().isLeftClick()) {
				shop.setDefaultShopMode(shop.getDefaultShopMode().getNext());
				shopEditor.openInventory(clickContext.getPlayer(), shopMode.getNext(), shopPage);
			}
		});
		setItemAndClickHandlerBottom(0, 4, getButton(shopEditor.isFreezeItems(), Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
				Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE), clickContext -> {
			shopEditor.setFreezeItems(!shopEditor.isFreezeItems());
			setItemBottom(0, 4, getButton(shopEditor.isFreezeItems(), Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
					Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE));
			refresh(clickContext.getPlayer(), 4 + INDEX_DIFFERENCE); //TODO refresht nicht
		});
		setItemAndClickHandlerBottom(0, 7, ItemStackUtils.createCustomHead(ItemStackUtils.HEAD_URL_LETTER_T,
				Message.MANAGER_GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME, Message.MANAGER_GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE), clickContext -> {
			refresh(shop.applyTemplate(TemplateHandler.getInstance().getDefaultTemplate()));
		});
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareMenu();
		return super.openInventorySync(player, inventoryPreparer);
	}

	private ItemStack getButton(boolean val, Message name, Message lore) {
		return ItemStackUtils.createItemStack(val ? Material.LIME_DYE : Material.GRAY_DYE,
				name.getTranslation(Template.of("value", val + "")),
				lore.getTranslations(Template.of("value", val + "")));
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
}
