package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.TemplateHandler;
import de.bossascrew.shops.general.menu.*;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.tr7zw.nbtapi.NBTItem;
import net.kyori.adventure.text.Component;
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

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.stream.IntStream;

public class ChestShopPageEditor extends BottomTopChestMenu implements EditorMenu<Player> {

	public static final String UUID_TAG_KEY = "shops-entry-uuid";
	public static final String IGNORE_TAG_KEY = "shops-entry-ignore";
	public static final String SLOT_TAG_KEY = "shops-entry-slot";

	private final ChestMenuShop shop;
	private final int shopPage;

	private final ChestShopEditor shopEditor;
	private final ContextConsumer<BackContext> backHandler;

	public ChestShopPageEditor(ChestMenuShop shop, int shopPage, ContextConsumer<BackContext> backHandler, ChestShopEditor shopEditor) {
		super(Component.empty(), shop.getRows(), 1);
		this.shop = shop;
		this.shopPage = shopPage;
		this.shopEditor = shopEditor;
		this.backHandler = backHandler;
		//Save all changed items before closing menu
		this.closeHandler = closeContext -> {
			setEditor(null);
			shop.saveToDatabase();
			if (!shopEditor.isFreezeItems()) {
				shopEditor.setFreezeItems(true);
				handleFreeze();
			}
		};
		this.refreshTitle();
	}

	private void refreshTitle() {
		super.setTitle(Message.SHOP_GUI_TITLE.getTranslation(
				Template.of("name", shop.getName()),
				Template.of("page-title", shop.getPageTitle(shopPage)),
				Template.of("page", "" + (shopPage + 1)),
				Template.of("pages", "" + Integer.max(shop.getPageCount(), shopPage + 1))));
	}

	private void prepareMenu() {

		insertFrozenEntries();
		if (getSpecialItems().size() == 0) {
			EntryTemplate template = shop.getDefaultTemplate();
			if (template != null) {
				for (Map.Entry<Integer, ShopEntry> entry : template.getEntries(shop.getRows()).entrySet()) {
					ItemStack actualSlot = getItemStack(entry.getKey());
					if (actualSlot == null || actualSlot.getType() == Material.AIR) {
						ItemStack newItem = entry.getValue().getDisplayItem();
						NBTItem nbtItem = new NBTItem(newItem);
						nbtItem.setBoolean(IGNORE_TAG_KEY, true);
						nbtItem.setInteger(SLOT_TAG_KEY, entry.getKey());
						setItem(entry.getKey(), nbtItem.getItem());
					}
				}
			}
		}

		setDefaultClickHandler(clickContext -> {
			ShopEntry clickedEntry = shop.getEntry(clickContext.getSlot() + shopPage * RowedOpenableMenu.LARGEST_INV_SIZE);
			clickContext.setCancelled(clickContext.getSlot() >= BottomTopChestMenu.INDEX_DIFFERENCE || shopEditor.isFreezeItems());

			if (shopEditor.isFreezeItems()) {

				// Open the editor for the clicked ShopEntry, no matter what item in hand
				if (clickedEntry != null) {
					new EntryEditor(clickedEntry, backHandler -> shopEditor.openInventory(clickContext.getPlayer(), shopPage))
							.openInventory(clickContext.getPlayer());
				}
			}
		});
		fillBottom();
		setBackSlotBottom(8);
		setBackHandlerAction(backHandler);
		setItemAndClickHandlerBottom(0, 0, shopPage > 0 ? DefaultSpecialItem.PREV_PAGE_RP : DefaultSpecialItem.PREV_PAGE_OFF_RP, clickContext -> {
			shopEditor.openInventory(clickContext.getPlayer(), shopPage > 0 ? shopPage - 1 : shopPage);
		});
		setItemAndClickHandlerBottom(0, 1, DefaultSpecialItem.NEXT_PAGE_RP, clickContext -> {
			shopEditor.openInventory(clickContext.getPlayer(), shopPage + 1);
		});

		int templateSlot = 7;
		ItemStack templateStack = ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
				Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME, Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE);

		setItemAndClickHandlerBottom(0, 4, ItemStackUtils.createButtonItemStack(!shopEditor.isFreezeItems(), Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
				Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE), clickContext -> {

			if (shopEditor.isFreezeItems()) {
				handleUnfreeze();
				setItemBottom(templateSlot, DefaultSpecialItem.EMPTY_DARK.createSpecialItem());
				refresh(clickContext.getPlayer(), templateSlot);

			} else {
				handleFreeze();
				insertFrozenEntries();
				setItemBottom(templateSlot, templateStack); //TODO klappt noch nicht
				refresh(clickContext.getPlayer(), templateSlot);
			}

			shopEditor.setFreezeItems(!shopEditor.isFreezeItems());
			setItemBottom(0, 4, ItemStackUtils.createButtonItemStack(!shopEditor.isFreezeItems(), Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
					Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE));
			refresh(clickContext.getPlayer(), 4 + INDEX_DIFFERENCE + ROW_SIZE);
		});
		setItemAndClickHandlerBottom(0, 6, ItemStackUtils.createItemStack(Material.ANVIL, Message.GUI_SHOP_EDITOR_PAGE_TITLE_NAME.getTranslation(),
				Message.GUI_SHOP_EDITOR_PAGE_TITLE_LORE.getTranslations(Template.of("current", shop.getPageTitle(shopPage)))), clickContext -> {
			new AnvilGUI.Builder()
					.plugin(StatShops.getInstance())
					.text(shop.getPageTitleFormat(shopPage))
					.title(Message.GUI_SHOP_EDITOR_PAGE_TITLE_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(p), 1L))
					.onComplete((p, s) -> {
						shop.setPageTitle(shopPage, s);
						refreshTitle();
						openInventory(p);
						return AnvilGUI.Response.close();
					}).open(clickContext.getPlayer());
		});
		setItemAndClickHandlerBottom(0, templateSlot, templateStack, clickContext -> {
			if (!shopEditor.isFreezeItems()) {
				return;
			}
			if (clickContext.getAction().isRightClick()) {
				clickContext.getPlayer().closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("name")
						.title(Message.GUI_TEMPLATES_NEW.getLegacyTranslation())
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(p), 1L))
						.onComplete((p, s) -> {
							if (TemplateHandler.getInstance().createNew(s, shop, shopPage) == null) {
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
		for (int i = 0; i < getRowCount() * 9; i++) {
			setItem(i, null);
		}
		for (ShopEntry entry : shop.getEntries(shopPage)) {
			// Set uuid tag from entry
			NBTItem nbtItem = new NBTItem(entry.getDisplayItem().clone());
			nbtItem.setUUID(UUID_TAG_KEY, entry.getUUID());
			nbtItem.setInteger(SLOT_TAG_KEY, entry.getSlot());

			//Put item in unfrozen inventory
			setItem(entry.getSlot(), nbtItem.getItem());
		}
		refresh(IntStream.range(0, getRowCount() * 9).toArray());
	}

	private void handleFreeze() {
		List<ShopEntry> containedEntries = shop.getEntries(shopPage);

		boolean wasEmpty = containedEntries.isEmpty();
		boolean isEmpty = true;
		for (int slot : getSlots()) {
			// Only process items in upper inventory
			if (slot >= INDEX_DIFFERENCE) {
				continue;
			}
			int rawSlot = shopPage * LARGEST_INV_SIZE + slot;
			// Only process not empty stacks
			ItemStack stack = getItemStack(slot);
			if (stack == null || stack.getType() == Material.AIR) {
				continue;
			}
			NBTItem nbtItem = new NBTItem(stack);
			Integer taggedSlot = nbtItem.getInteger(SLOT_TAG_KEY);
			if (nbtItem.getBoolean(IGNORE_TAG_KEY) && taggedSlot != null && taggedSlot == slot) {
				continue;
			}

			UUID uuid = null;
			if (nbtItem.hasKey(UUID_TAG_KEY)) {
				uuid = nbtItem.getUUID(UUID_TAG_KEY);
			}
			if (uuid == null) {
				shop.createEntry(stack, rawSlot);
			} else {
				UUID finalUuid = uuid;
				ShopEntry entry = containedEntries.stream().filter(e -> e.getUUID().equals(finalUuid)).findFirst().orElse(null);
				if (entry == null) {
					ShopEntry unusedEntry = shop.getUnusedEntry(uuid);
					if (unusedEntry != null) {

						unusedEntry.setSlot(rawSlot);
						shop.addEntry(unusedEntry, rawSlot);
					}
				} else {
					shop.moveEntry(entry, rawSlot);
					containedEntries.remove(entry);
				}
			}
			isEmpty = false;
		}
		if (wasEmpty && !isEmpty && shop.getDefaultTemplate() != null) {
			shop.applyDefaultTemplate(shop.getDefaultTemplate(), shopPage);
		}
		containedEntries.forEach(shop::setEntryUnused);
	}

	private void insertFrozenEntries() {
		for (int i = 0; i < getRowCount() * 9; i++) {
			setItem(i, null);
		}
		for (ShopEntry entry : shop.getEntries(shopPage)) {
			setItem(entry.getSlot(), ItemStackUtils.createEntryItemStack(entry, null));
		}
		refresh(IntStream.range(0, getRowCount() * 9).toArray());
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareMenu();
		return super.openInventorySync(player, inventoryPreparer);
	}

	public void openTemplatesListMenu(Player player) {
		ListMenu<EntryTemplate> menu = new ListMenu<>(3, TemplateHandler.getInstance(),
				Message.GUI_TEMPLATES_CHOOSE, backHandler -> openInventory(player));
		menu.setClickHandler(clickContext -> {
			if(clickContext.getAction().isRightClick()) {
				clickContext.getTarget().setDiscIndex((short) ((clickContext.getTarget().getDiscIndex() + 1) % TemplateHandler.DISCS.length));
				openTemplatesListMenu(player);
				return;
			}
			openTemplateApplyMenu(player, clickContext.getTarget());
		});
		menu.openInventory(player);
	}

	public void openTemplateApplyMenu(Player player, EntryTemplate template) {
		BottomTopChestMenu menu = new BottomTopChestMenu(Message.GUI_TEMPLATES_APPLY.getTranslation(Template.of("name", template.getName())), shop.getRows(), 1);
		menu.fillMenu(DefaultSpecialItem.EMPTY_LIGHT_RP);
		menu.fillBottom();
		int dif = shopPage * INDEX_DIFFERENCE;
		for (int i = 0; i < shop.getRows() * ROW_SIZE; i++) {
			ShopEntry entry = shop.getEntry(i + dif);
			if (entry == null) {
				continue;
			}
			menu.setItem(i, entry.getDisplayItem());
		}
		for (ShopEntry entry : template.getEntries(shop.getRows()).values()) {
			menu.setItem(entry.getSlot(), entry.getDisplayItem());
		}
		menu.setItemAndClickHandlerBottom(ROW_SIZE + 2, DefaultSpecialItem.ACCEPT_RP, clickContext -> {
			shop.applyTemplate(template, shopPage);
			openInventory(player);
		});
		menu.setItemAndClickHandlerBottom(ROW_SIZE + 6, DefaultSpecialItem.DECLINE_RP, clickContext -> openInventory(player));
		menu.setItemBottom(0, 5, DefaultSpecialItem.EMPTY_DARK_RP.createSpecialItem());
		menu.openInventory(player);
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
