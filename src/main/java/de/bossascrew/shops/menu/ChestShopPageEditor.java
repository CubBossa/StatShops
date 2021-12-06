package de.bossascrew.shops.menu;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.TemplateHandler;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.EntryTemplate;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.util.LoggingPolicy;
import de.tr7zw.nbtapi.NBTItem;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
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
import java.util.function.Function;

public class ChestShopPageEditor extends BottomTopChestMenu {

	public static final String UUID_TAG_KEY = "shops-entry-uuid";

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
		//Save all changes items before closing menu
		this.closeHandler = closeContext -> handleFreeze();
	}

	private void prepareMenu() {
		for (int i = shopPage * RowedOpenableMenu.LARGEST_INV_SIZE; i < (shopPage + 1) * RowedOpenableMenu.LARGEST_INV_SIZE; i++) {
			ShopEntry entry = shop.getEntry(shopMode, i);
			if (entry == null) {
				continue;
			}
			setItem(i - shopPage * RowedOpenableMenu.LARGEST_INV_SIZE, ItemStackUtils.prepareEditorEntryItemStack(entry));
		}
		setDefaultClickHandler(ClickType.LEFT, clickContext -> {
			ShopEntry clickedEntry = shop.getEntry(shopMode, clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
			clickContext.setCancelled(shopEditor.isFreezeItems());

			if (shopEditor.isFreezeItems()) {

				// Open the editor for the clicked ShopEntry, no matter what item in hand
				if (clickedEntry != null) {
					new EntryEditor(clickedEntry, backHandler -> shopEditor.openInventory(clickContext.getPlayer(), shopMode, shopPage))
							.openInventory(clickContext.getPlayer());
				}
			} else if (clickContext.getAction() == ClickType.MIDDLE) {
				//TODO clone clicked stack in hand
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
		setItemAndClickHandlerBottom(0, 4, getButton(!shopEditor.isFreezeItems(), Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
				Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE), clickContext -> {

			if (shopEditor.isFreezeItems()) {
				handleUnfreeze();
			} else {
				handleFreeze();
			}

			shopEditor.setFreezeItems(!shopEditor.isFreezeItems());
			setItemBottom(0, 4, getButton(!shopEditor.isFreezeItems(), Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
					Message.MANAGER_GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE));
			refresh(clickContext.getPlayer(), 4 + INDEX_DIFFERENCE + ROW_SIZE);
		});
		setItemAndClickHandlerBottom(0, 7, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
				Message.MANAGER_GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME, Message.MANAGER_GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE), clickContext -> {
			if (clickContext.getAction().isRightClick()) {
				clickContext.getPlayer().closeInventory();
				new AnvilGUI.Builder()
						.plugin(ShopPlugin.getInstance())
						.text("name")
						.title(Message.MANAGER_GUI_TEMPLATES_NEW.getLegacyTranslation())
						.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openInventory(p), 1L))
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
				ShopPlugin.getInstance().log(LoggingPolicy.WARN, "Item was in menu but no entry was found.");
				continue;
			}
			// Set uuid tag from entry
			NBTItem nbtItem = new NBTItem(stack);
			nbtItem.setUUID(UUID_TAG_KEY, entry.getUUID());

			//Put item in unfrozen inventory
			setItem(slot, nbtItem.getItem());
			refresh(slot);
		}
	}

	private void handleFreeze() {
		List<ShopEntry> containedEntries = shop.getEntries(shopMode, shopPage);

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
				Message.MANAGER_GUI_TEMPLATES_CHOOSE, backHandler -> openInventory(player));
		menu.setClickHandler(clickContext -> openTemplateApplyMenu(player, clickContext.getTarget()));
		menu.openInventory(player);
	}

	public void openTemplateApplyMenu(Player player, EntryTemplate template) {
		BottomTopChestMenu menu = new BottomTopChestMenu(Message.MANAGER_GUI_TEMPLATES_APPLY.getTranslation(Template.of("name", template.getName())), shop.getRows(), 1);
		menu.fillMenu(DefaultSpecialItem.EMPTY_LIGHT);
		menu.fillBottom();
		int dif = shopPage * INDEX_DIFFERENCE;
		for (int i = 0; i < shop.getRows() + ROW_SIZE; i++) {
			ShopEntry entry = shop.getEntry(shopMode, i + dif);
			if (entry == null) {
				continue;
			}
			menu.setItem(i, entry.getDisplayItem());
		}
		for (Map.Entry<Function<Integer, Integer>, ShopEntry> mapEntry : template.entrySet()) {
			int slot = mapEntry.getKey().apply(getRowCount());
			ShopEntry entry = mapEntry.getValue();
			menu.setItem(slot, entry.getDisplayItem());
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
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getPrevious().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getDisplayName())));
		lore.add(Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_LORE.getTranslation(Template.of("mode", shopMode.getNext().getDisplayName().color(NamedTextColor.GRAY))));
		lore.add(Component.text("...", NamedTextColor.DARK_GRAY));
		return ItemStackUtils.createItemStack(modeItem.getType(),
				Message.MANAGER_GUI_SHOP_SET_DEFAULT_MODE_NAME.getTranslation(Template.of("name", modeItem.getItemMeta().getDisplayName())),
				lore);
	}
}
