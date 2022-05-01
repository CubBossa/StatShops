package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.TemplateHandler;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.entry.BaseEntry;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.guiframework.inventory.*;
import de.cubbossa.guiframework.inventory.implementations.AnvilMenu;
import de.cubbossa.guiframework.inventory.implementations.BottomInventoryMenu;
import de.cubbossa.guiframework.inventory.implementations.ListMenu;
import de.cubbossa.guiframework.inventory.implementations.RectInventoryMenu;
import de.tr7zw.changeme.nbtapi.NBTItem;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.*;
import java.util.stream.IntStream;

public class ChestShopEditor extends RectInventoryMenu {

	public static final String UUID_TAG_KEY = "shops-entry-uuid";
	public static final String IGNORE_TAG_KEY = "shops-entry-ignore";
	public static final String SLOT_TAG_KEY = "shops-entry-slot";

	private final ChestMenuShop shop;
	private final int shopPage;
	private boolean freezeItems = true;

	private BottomInventoryMenu bottomMenu;

	public ChestShopEditor(ChestMenuShop shop, int shopPage) {
		super(Component.empty(), shop.getRows());
		this.shop = shop;
		this.shopPage = shopPage;

		//Save all changed items before closing menu
		this.closeHandler = closeContext -> {
			Collection<ShopEntry> unused = shop.getUnusedEntries();
			if (unused.size() > 0) {
				Customer.wrap(closeContext.getPlayer()).sendMessage(Message.GUI_SHOP_EDITOR_UNUSED_INFO.getKey(), Message.GUI_SHOP_EDITOR_UNUSED_INFO.getTranslation(
						TagResolver.resolver("amount", Tag.inserting(Component.text(unused.size() + "")))));
			}
			if (!freezeItems) {
				freezeItems = true;
				handleFreeze();
			}
		};
		this.refreshTitle();
	}

	@Override
	public void openSync(Player viewer, ViewMode viewMode) {
		clearContent();
		prepareMenu(viewer);
		bottomMenu.open(viewer);
		super.openSync(viewer, viewMode);
	}

	@Override
	public void close(Player viewer) {
		super.close(viewer);
		bottomMenu.close(viewer);
	}

	private void refreshTitle() {
		updateTitle(Message.SHOP_GUI_TITLE.getTranslation(
						TagResolver.resolver("name", Tag.inserting(shop.getName())),
						TagResolver.resolver("page-title", Tag.inserting(shop.getPageTitle(shopPage))),
						TagResolver.resolver("page", Tag.inserting(Component.text("" + (shopPage + 1)))),
						TagResolver.resolver("pages", Tag.inserting(Component.text(Integer.max(shop.getPageCount(), shopPage + 1))))),
				shopPage);
	}

	private void prepareMenu(Player player) {

		bottomMenu = new BottomInventoryMenu(InventoryRow.FIRST_ROW);

		insertFrozenEntries(player);

		setDefaultClickHandler(c -> {
			ShopEntry clickedEntry = shop.getEntry(c.getSlot() + shopPage * (9 * 6));
			c.setCancelled(freezeItems);

			if (freezeItems && clickedEntry != null) {
				new EntryEditor(clickedEntry, c.getPlayer()).open(c.getPlayer());
			}
		});

		bottomMenu.addPreset(MenuPresets.fill(MenuPresets.FILLER_DARK));
		bottomMenu.setItem(9 + 5, Icon.EMPTY_DARK_RP.create());

		bottomMenu.setButton(17, Button.builder()
				.withItemStack(Icon.BACK.create())
				.withClickHandler(Action.LEFT, c -> close(c.getPlayer())));

		bottomMenu.setButton(9, Button.builder()
				.withItemStack(() -> shopPage > 0 ? Icon.PREV_PAGE_RP.create() : Icon.PREV_PAGE_OFF_RP.create())
				.withClickHandler(Action.LEFT, c -> {
					setPreviousPage(c.getPlayer());
					c.getMenu().refresh(c.getSlot());
				}));
		bottomMenu.setButton(10, Button.builder()
				.withItemStack(Icon.NEXT_PAGE_RP.create())
				.withClickHandler(Action.LEFT, clickContext -> setNextPage(clickContext.getPlayer())));

		int nameSlot = 9 + 6;
		int templateSlot = 9 + 7;

		bottomMenu.setButton(4, Button.builder()
				.withItemStack(() -> ItemStackUtils.createButtonItemStack(!freezeItems,
						Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME,
						Message.GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE))
				.withClickHandler(Action.LEFT, clickContext -> {
					if (freezeItems) {
						handleUnfreeze();
					} else {
						handleFreeze();
						insertFrozenEntries(clickContext.getPlayer());
					}
					freezeItems = !freezeItems;
					bottomMenu.refresh(templateSlot);
				}));

		bottomMenu.setButton(nameSlot, Button.builder()
				.withItemStack(ItemStackUtils.createItemStack(Material.ANVIL,
						Message.GUI_SHOP_EDITOR_PAGE_TITLE_NAME,
						Message.GUI_SHOP_EDITOR_PAGE_TITLE_LORE.getTranslations(TagResolver.resolver("current", Tag.inserting(shop.getPageTitle(shopPage))))))
				.withClickHandler(Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
					AnvilMenu m = new AnvilMenu(Message.GUI_SHOP_EDITOR_PAGE_TITLE_TITLE, shop.getPageTitleFormat(shopPage));
					m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
						shop.setPageTitle(shopPage, s.getTarget());
						s.getPlayer().closeInventory();
					});
					return m;
				})));

		bottomMenu.setButton(templateSlot, Button.builder()
				.withItemStack(() -> freezeItems ? ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
						Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME,
						Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE) : Icon.EMPTY_DARK.create())
				.withClickHandler(Action.LEFT, c -> {
					if (freezeItems) {
						c.getMenu().openSubMenu(c.getPlayer(), newTemplatesListMenu());
					}
				})
				.withClickHandler(Action.RIGHT, c -> {
					if (!freezeItems) {
						return;
					}
					c.getMenu().openSubMenu(c.getPlayer(), () -> {
						AnvilMenu m = new AnvilMenu(Message.GUI_TEMPLATES_NEW, "name");
						m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
							if (TemplateHandler.getInstance().createNew(s.getTarget(), shop, shopPage) == null) {
								s.getPlayer().playSound(s.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
								return;
							}
							openSubMenu(s.getPlayer(), newTemplatesListMenu());
							s.getPlayer().closeInventory();
						});
						return m;
					});
					c.getPlayer().closeInventory();
				}));
	}

	private void handleUnfreeze() {
		for (int i = 0; i < getRows() * 9; i++) {
			removeItem(i);
		}
		for (ShopEntry entry : shop.getEntries(shopPage)) {
			// Set uuid tag from entry
			NBTItem nbtItem = new NBTItem(entry.getDisplayItem().clone());
			nbtItem.setUUID(UUID_TAG_KEY, entry.getUUID());
			nbtItem.setInteger(SLOT_TAG_KEY, entry.getSlot());

			//Put item in unfrozen inventory
			setItem(entry.getSlot() % (9 * 6), nbtItem.getItem());
		}
		insertTemplateOverlay();
		refresh(IntStream.range(0, getRows() * 9).toArray());
	}

	private void handleFreeze() {
		List<ShopEntry> containedEntries = shop.getEntries(shopPage);
		Map<Integer, BaseEntry> newEntryPositions = new TreeMap<>();

		boolean wasEmpty = containedEntries.isEmpty();
		boolean isEmpty = true;
		boolean ignore = true;

		for (int slot : getSlots()) {
			//code twice :p hew

			int rawSlot = shopPage * 9 * 6 + slot;
			// Only process not empty stacks
			ItemStack stack = getItemStack(slot);
			if (stack == null || stack.getType() == Material.AIR) {
				continue;
			}
			NBTItem nbtItem = new NBTItem(stack);
			Integer taggedSlot = nbtItem.getInteger(SLOT_TAG_KEY);
			if (nbtItem.getBoolean(IGNORE_TAG_KEY) && (taggedSlot == null || taggedSlot != rawSlot)) {
				ignore = false;
				break;
			}
		}

		for (int slot : getSlots()) {

			int rawSlot = shopPage * 9 * 6 + slot;
			// Only process not empty stacks
			ItemStack stack = getItemStack(slot);
			if (stack == null || stack.getType() == Material.AIR) {
				continue;
			}
			NBTItem nbtItem = new NBTItem(stack);
			if (ignore && nbtItem.getBoolean(IGNORE_TAG_KEY)) {
				continue;
			}

			UUID uuid = null;
			if (nbtItem.hasKey(UUID_TAG_KEY)) {
				uuid = nbtItem.getUUID(UUID_TAG_KEY);
			}

			//cleanup item
			nbtItem.removeKey(UUID_TAG_KEY);
			nbtItem.removeKey(SLOT_TAG_KEY);
			nbtItem.removeKey(IGNORE_TAG_KEY);
			stack = nbtItem.getItem();

			if (uuid == null) {
				// create new but dont store -> might override old entry
				newEntryPositions.put(rawSlot, new BaseEntry(UUID.randomUUID(), shop, stack, null, rawSlot));
			} else {
				ShopEntry entry = shop.getEntry(uuid);
				if (entry == null) {
					// create new but dont store -> might override old entry
					newEntryPositions.put(rawSlot, new BaseEntry(uuid, shop, stack, null, rawSlot));
				} else {
					containedEntries.remove(entry);
					// move entry to position because it cannot override anything
					shop.moveEntry(entry, rawSlot);
				}
			}
			isEmpty = false;
		}
		for (Map.Entry<Integer, BaseEntry> mapEntry : newEntryPositions.entrySet()) {
			shop.addEntry(mapEntry.getValue(), mapEntry.getKey());
		}
		if (ignore && wasEmpty && !isEmpty && shop.getDefaultTemplate() != null) {
			shop.applyDefaultTemplate(shop.getDefaultTemplate(), shopPage);
		}
		containedEntries.forEach(shop::setEntryUnused);

		if (shop.getPageCount() < shop.getDefaultShopPage()) {
			shop.setDefaultPage(shop.getPageCount());
		}
	}

	private void insertFrozenEntries(Player player) {
		for (int i = 0; i < getRows() * 9; i++) {
			removeItem(i);
		}
		for (ShopEntry entry : shop.getEntries(shopPage)) {
			setItem(entry.getSlot() % (9 * 6), ItemStackUtils.createEntryItemStack(entry, Customer.wrap(player)));
		}
		insertTemplateOverlay();
		refresh(IntStream.range(0, getRows() * 9).toArray());
	}

	private void insertTemplateOverlay() {
		if (shop.getEntries(shopPage).size() == 0) {
			EntryTemplate template = shop.getDefaultTemplate();
			if (template != null) {
				for (Map.Entry<Integer, ShopEntry> entry : template.getEntries(shop.getRows()).entrySet()) {
					ItemStack actualSlot = getItemStack(entry.getKey());
					if (actualSlot == null || actualSlot.getType() == Material.AIR) {
						ItemStack newItem = entry.getValue().getDisplayItem();
						NBTItem nbtItem = new NBTItem(newItem);
						nbtItem.setBoolean(IGNORE_TAG_KEY, true);
						nbtItem.setInteger(SLOT_TAG_KEY, entry.getKey() + shopPage * (9 * 6));
						setItem(entry.getKey(), nbtItem.getItem());
					}
				}
			}
		}
	}

	public Menu newTemplatesListMenu() {
		ListMenu menu = new ListMenu(Message.GUI_TEMPLATES_CHOOSE, 3);
		for (EntryTemplate template : TemplateHandler.getInstance().getTemplates()) {
			menu.addListEntry(Button.builder()
					.withItemStack(template.getListDisplayItem())
					.withClickHandler(Action.LEFT, c -> {
						template.setDiscIndex((short) ((template.getDiscIndex() + 1) % TemplateHandler.DISCS.length));
						c.getMenu().refresh(c.getSlot());
					})
					.withClickHandler(Action.RIGHT, c -> c.getMenu().openSubMenu(c.getPlayer(), newTemplateApplyMenu(c.getPlayer(), template))));
		}
		return menu;
	}

	public Menu newTemplateApplyMenu(Player player, EntryTemplate template) {
		RectInventoryMenu menu = new RectInventoryMenu(Message.GUI_TEMPLATES_APPLY.getTranslation(TagResolver.resolver("name", Tag.inserting(template.getName()))), shop.getRows());
		BottomInventoryMenu bottomMenu = new BottomInventoryMenu(InventoryRow.FIRST_ROW);

		bottomMenu.addPreset(MenuPresets.fill(Icon.EMPTY_DARK_RP.create()));
		menu.addPreset(MenuPresets.fill(Icon.EMPTY_LIGHT_RP.create()));

		int dif = shopPage * 9 * 6;
		IntStream.range(0, shop.getRows() * 9).forEach(value -> {
			ShopEntry entry = shop.getEntry(value + dif);
			if (entry == null) {
				return;
			}
			menu.setItem(value, entry.getDisplayItem());
		});

		for (ShopEntry entry : template.getEntries(shop.getRows()).values()) {
			menu.setItem(entry.getSlot(), entry.getDisplayItem());
		}
		bottomMenu.setButton(9 + 2, Button.builder()
				.withItemStack(Icon.ACCEPT_RP.create())
				.withClickHandler(Action.LEFT, clickContext -> {
					shop.applyTemplate(template, shopPage);
					open(clickContext.getPlayer());
				}));
		menu.setButton(9 + 6, Button.builder()
				.withItemStack(Icon.DECLINE_RP.create())
				.withClickHandler(Action.LEFT, clickContext -> open(player)));
		menu.setItem(9 + 5, Icon.EMPTY_DARK_RP.create());
		return menu;
	}
}
