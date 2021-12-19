package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.TemplateHandler;
import de.bossascrew.shops.general.menu.*;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.ShopMode;
import de.tr7zw.nbtapi.NBTItem;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

public class ChestShopPageEditor extends BottomTopChestMenu implements EditorMenu<Player> {

	public static final String UUID_TAG_KEY = "shops-entry-uuid";
	public static final String IGNORE_TAG_KEY = "shops-entry-ignore";

	private final ChestMenuShop shop;
	private final ShopMode shopMode;
	private final int shopPage;

	private final ChestShopEditor shopEditor;
	private final ContextConsumer<BackContext> backHandler;

	public ChestShopPageEditor(ChestMenuShop shop, ShopMode shopMode, int shopPage, ContextConsumer<BackContext> backHandler, ChestShopEditor shopEditor) {
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
		//Save all changed items before closing menu
		this.closeHandler = closeContext -> {
			if (!shopEditor.isFreezeItems()) {
				shopEditor.setFreezeItems(true);
				handleFreeze();
			}
		};
	}

	private void prepareMenu() {
		boolean applyTemplatePreview = true;
		for (int i = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE; i < (shopPage + 1) * RowedOpenableMenu.LARGEST_INV_SIZE; i++) {
			ShopEntry entry = shop.getEntry(shopMode, i);
			if (entry == null) {
				continue;
			}
			ItemStack stack = ItemStackUtils.prepareEditorEntryItemStack(entry);
			NBTItem item = new NBTItem(stack);
			item.setUUID(UUID_TAG_KEY, entry.getUUID());
			stack = item.getItem();
			setItem(i - shopPage * RowedOpenableMenu.LARGEST_INV_SIZE, stack);
			applyTemplatePreview = false;
		}
		if (applyTemplatePreview) {
			EntryTemplate template = shop.getDefaultTemplate();
			if (template != null) {
				for (Map.Entry<Integer, ShopEntry> entry : template.getEntries(shop.getRows()).entrySet()) {
					ItemStack actualSlot = getItemStack(entry.getKey());
					if (actualSlot == null || actualSlot.getType() == Material.AIR) {
						ItemStack newItem = entry.getValue().getDisplayItem();
						NBTItem nbtItem = new NBTItem(newItem);
						nbtItem.setBoolean(IGNORE_TAG_KEY, true);
						setItem(entry.getKey(), nbtItem.getItem());
					}
				}
			}
		}

		setDefaultClickHandler(clickContext -> {
			ShopEntry clickedEntry = shop.getEntry(shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
			clickContext.setCancelled(shopEditor.isFreezeItems());

			if (shopEditor.isFreezeItems()) {

				// Open the editor for the clicked ShopEntry, no matter what item in hand
				if (clickedEntry != null) {
					new EntryEditor(clickedEntry, backHandler -> shopEditor.openInventory(clickContext.getPlayer(), shopMode, shopPage))
							.openInventory(clickContext.getPlayer());
				}
			}
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
		setItemAndClickHandlerBottom(0, 4, getButton(!shopEditor.isFreezeItems(), Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
				Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE), clickContext -> {

			if (shopEditor.isFreezeItems()) {
				handleUnfreeze();
			} else {
				handleFreeze();
			}

			shopEditor.setFreezeItems(!shopEditor.isFreezeItems());
			setItemBottom(0, 4, getButton(!shopEditor.isFreezeItems(), Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
					Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE));
			refresh(clickContext.getPlayer(), 4 + INDEX_DIFFERENCE + ROW_SIZE);
		});
		setItemAndClickHandlerBottom(0, 7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
				Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME, Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE), clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				clickContext.getPlayer().closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("name")
						.title(Message.GUI_TEMPLATES_NEW.getLegacyTranslation())
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(p), 1L))
						.onComplete((p, s) -> {
							if (TemplateHandler.getInstance().createNew(s, shop, shopMode, shopPage) == null) {
								p.playSound(p.getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
							}
							openTemplatesListMenu(p);
							return AnvilGUI.Response.close();
						}).open(clickContext.getPlayer());
			} else {
				openTemplatesListMenu(clickContext.getPlayer());
			}
		});
		setItemBottom(0, 5, DefaultSpecialItem.EMPTY_DARK_RP.createSpecialItem());
	}

	private void handleUnfreeze() {
		for (int slot : getSlots()) {
			// Only process items in upper inventory
			if (slot >= INDEX_DIFFERENCE) {
				continue;
			}
			// Only process not empty stacks
			ItemStack stack = getItemStack(slot);
			if (stack == null || stack.getType() == Material.AIR) {
				continue;
			}
			// Get entry from this slot
			ShopEntry entry = shop.getEntry(shopMode, slot + LARGEST_INV_SIZE * shopPage);
			if (entry == null) {
				// Must be a template entry
				continue;
			}
			// Set uuid tag from entry
			NBTItem nbtItem = new NBTItem(stack);
			Boolean ignore = nbtItem.getBoolean(IGNORE_TAG_KEY);
			if (ignore) {
				continue;
			}
			nbtItem.setUUID(UUID_TAG_KEY, entry.getUUID());

			//Put item in unfrozen inventory
			setItem(slot, nbtItem.getItem());
			refresh(slot);
		}
	}

	private void handleFreeze() {
		List<ShopEntry> containedEntries = shop.getEntries(shopMode, shopPage);

		boolean wasEmpty = containedEntries.isEmpty();
		boolean isEmpty = true;
		for (int slot : getSlots()) {
			// Only process items in upper inventory
			if (slot >= INDEX_DIFFERENCE) {
				continue;
			}
			int shopSlot = shopPage * LARGEST_INV_SIZE + slot;
			// Only process not empty stacks
			ItemStack stack = getItemStack(slot);
			if (stack == null || stack.getType() == Material.AIR) {
				continue;
			}
			NBTItem nbtItem = new NBTItem(stack);
			if (nbtItem.getBoolean(IGNORE_TAG_KEY)) {
				continue;
			}

			UUID uuid = null;
			if (nbtItem.hasKey(UUID_TAG_KEY)) {
				uuid = nbtItem.getUUID(UUID_TAG_KEY);
			}
			if (uuid == null) {
				shop.createEntry(stack, shopMode, shopSlot);
			} else {
				UUID finalUuid = uuid;
				ShopEntry entry = containedEntries.stream().filter(e -> e.getUUID().equals(finalUuid)).findFirst().orElse(null);
				if (entry == null) {
					ShopEntry unusedEntry = shop.getUnusedEntry(uuid);
					if (unusedEntry != null) {
						shop.addEntry(shopMode, shopSlot, unusedEntry);
					}
				} else {
					shop.moveEntry(entry, shopMode, shopSlot);
					containedEntries.remove(entry);
				}
			}
			isEmpty = false;
		}
		if (wasEmpty && !isEmpty && shop.getDefaultTemplate() != null) {
			shop.applyTemplate(shop.getDefaultTemplate(), shopMode, shopPage);
		}
		containedEntries.forEach(shop::setEntryUnused);
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareMenu();
		return super.openInventorySync(player, inventoryPreparer);
	}

	public void openTemplatesListMenu(Player player) {
		ListMenu<EntryTemplate> menu = new ListMenu<>(3, TemplateHandler.getInstance(),
				Message.GUI_TEMPLATES_CHOOSE, backHandler -> openInventory(player));
		menu.setClickHandler(clickContext -> openTemplateApplyMenu(player, clickContext.getTarget()));
		menu.openInventory(player);
	}

	public void openTemplateApplyMenu(Player player, EntryTemplate template) {
		BottomTopChestMenu menu = new BottomTopChestMenu(Message.GUI_TEMPLATES_APPLY.getTranslation(Template.of("name", template.getName())), shop.getRows(), 1);
		menu.fillMenu(DefaultSpecialItem.EMPTY_LIGHT);
		menu.fillBottom();
		int dif = shopPage * INDEX_DIFFERENCE;
		for (int i = 0; i < shop.getRows() * ROW_SIZE; i++) {
			ShopEntry entry = shop.getEntry(shopMode, i + dif);
			if (entry == null) {
				continue;
			}
			menu.setItem(i, entry.getDisplayItem());
		}
		for (ShopEntry entry : template.getEntries(shop.getRows()).values()) {
			menu.setItem(entry.getSlot(), entry.getDisplayItem());
		}
		menu.setItemAndClickHandlerBottom(ROW_SIZE + 2, DefaultSpecialItem.ACCEPT, clickContext -> {
			shop.applyTemplate(template, shopMode, shopPage);
			openInventory(player);
		});
		menu.setItemAndClickHandlerBottom(ROW_SIZE + 6, DefaultSpecialItem.DECLINE, clickContext -> openInventory(player));
		menu.openInventory(player);
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
		lore.add(Message.GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getPrevious().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getDisplayName())));
		lore.add(Message.GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getNext().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		return ItemStackUtils.createItemStack(modeItem.getType(),
				Message.GUI_SHOP_SET_DEFAULT_MODE_NAME.getTranslation(Template.of("name", modeItem.getItemMeta().getDisplayName())),
				lore);
	}

	@Override
	public boolean isEditorSet() {
		return shop.getEditor() != null;
	}

	@Override
	public boolean setEditor(Player editor) {
		if (isEditorSet()) {
			return false;
		}
		shop.setEditor(editor);
		return true;
	}

	@Override
	public Player getEditor() {
		return shop.getEditor();
	}
}
