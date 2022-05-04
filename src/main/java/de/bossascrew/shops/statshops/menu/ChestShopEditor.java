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
	private boolean freezeItems = true;

	private BottomInventoryMenu bottomMenu;

	public ChestShopEditor(ChestMenuShop shop) {
		super(Component.empty(), shop.getRows());
		this.shop = shop;

		//Save all changed items before closing menu
		this.closeHandler = closeContext -> {
			Collection<ShopEntry> unused = shop.getUnusedEntries();
			if (unused.size() > 0) {
				Customer.wrap(closeContext.getPlayer()).sendMessage(Message.GUI_SHOP_EDITOR_UNUSED_INFO.getKey(), Message.GUI_SHOP_EDITOR_UNUSED_INFO.asComponent(
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
	public void handleClose(Player viewer) {
		super.handleClose(viewer);
		bottomMenu.close(viewer);
	}

	private void refreshTitle() {
		updateTitle(Message.SHOP_GUI_TITLE.asComponent(
						TagResolver.resolver("name", Tag.inserting(shop.getName())),
						TagResolver.resolver("page-title", Tag.inserting(shop.getPageTitle(getCurrentPage()))),
						TagResolver.resolver("page", Tag.inserting(Component.text("" + (getCurrentPage() + 1)))),
						TagResolver.resolver("pages", Tag.inserting(Component.text(Integer.max(shop.getPageCount(), getCurrentPage() + 1))))),
				getCurrentPage());
	}

	private void prepareMenu(Player player) {

		bottomMenu = new BottomInventoryMenu(InventoryRow.FIRST_ROW);

		freezeItems = true;
		insertFrozenEntries(player);

		setDefaultClickHandler(c -> {
			ShopEntry clickedEntry = shop.getEntry(c.getSlot() + getCurrentPage() * (9 * 6));
			c.setCancelled(freezeItems);

			if (freezeItems && clickedEntry != null) {
				new ShopEntryEditor(clickedEntry, c.getPlayer()).open(c.getPlayer());
			}
		});

		bottomMenu.addPreset(MenuPresets.fill(Icon.EMPTY_LIGHT_RP.create()));
		bottomMenu.addPreset(buttonHandler -> buttonHandler.addItem(9 + 5, Icon.EMPTY_DARK_RP.create()));

		bottomMenu.setButton(17, Button.builder()
				.withItemStack(Icon.BACK.create())
				.withClickHandler(Action.LEFT, c -> openPreviousMenu(c.getPlayer())));

		bottomMenu.setButton(9, Button.builder()
				.withItemStack(() -> getCurrentPage() > 0 ? Icon.PREV_PAGE_RP.create() : Icon.PREV_PAGE_OFF_RP.create())
				.withClickHandler(Action.LEFT, c -> {
					if (getCurrentPage() <= 0) {
						return;
					}
					setPreviousPage(c.getPlayer());
					c.getMenu().refresh(c.getSlot());
					refreshTitle();
				}));
		bottomMenu.setButton(10, Button.builder()
				.withItemStack(Icon.NEXT_PAGE_RP.create())
				.withClickHandler(Action.LEFT, c -> {
					setNextPage(c.getPlayer());
					c.getMenu().refresh(c.getSlot() - 1);
					refreshTitle();
				}));

		int nameSlot = 9 + 6;
		int templateSlot = 9 + 7;

		bottomMenu.setButton(9 + 4, Button.builder()
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
					bottomMenu.refresh(clickContext.getSlot(), templateSlot);
				}));

		bottomMenu.setButton(nameSlot, Button.builder()
				.withItemStack(() -> ItemStackUtils.createItemStack(Material.ANVIL,
						Message.GUI_SHOP_EDITOR_PAGE_TITLE_NAME,
						Message.GUI_SHOP_EDITOR_PAGE_TITLE_LORE.asComponents(TagResolver.resolver("current", Tag.inserting(shop.getPageTitle(getCurrentPage()))))))
				.withClickHandler(Action.LEFT, c -> openSubMenu(c.getPlayer(), () -> {
					AnvilMenu m = MainMenu.newAnvilMenu(Message.GUI_SHOP_EDITOR_PAGE_TITLE_TITLE, shop.getPageTitleFormat(getCurrentPage()));
					m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
						shop.setPageTitle(getCurrentPage(), s.getTarget());
						c.getMenu().refresh(c.getSlot());
						s.getMenu().openPreviousMenu(s.getPlayer());
					});
					return m;
				})));

		bottomMenu.setButton(templateSlot, Button.builder()
				.withItemStack(() -> freezeItems ? ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TEMPLATE,
						Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME,
						Message.GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE) : Icon.EMPTY_DARK.create())
				.withClickHandler(Action.LEFT, c -> {
					if (freezeItems) {
						openSubMenu(c.getPlayer(), newTemplatesListMenu());
					}
				})
				.withClickHandler(Action.RIGHT, c -> {
					if (!freezeItems) {
						return;
					}
					c.getMenu().openSubMenu(c.getPlayer(), () -> {
						AnvilMenu m = MainMenu.newAnvilMenu(Message.GUI_TEMPLATES_NEW, "name");
						m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
							if (TemplateHandler.getInstance().createNew(s.getTarget(), shop, getCurrentPage()) == null) {
								s.getPlayer().playSound(s.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
								return;
							}
							openSubMenu(s.getPlayer(), newTemplatesListMenu());
							s.getMenu().openPreviousMenu(s.getPlayer());
						});
						return m;
					});
					c.getMenu().openPreviousMenu(c.getPlayer());
				}));
	}

	private void handleUnfreeze() {
		for (int i = 0; i < getRows() * 9; i++) {
			removeItem(i);
		}
		for (ShopEntry entry : shop.getEntries(getCurrentPage())) {
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
		List<ShopEntry> containedEntries = shop.getEntries(getCurrentPage());
		Map<Integer, BaseEntry> newEntryPositions = new TreeMap<>();

		boolean wasEmpty = containedEntries.isEmpty();
		boolean isEmpty = true;
		boolean ignore = true;

		for (int slot : getSlots()) {
			//code twice :p hew

			int rawSlot = getCurrentPage() * 9 * 6 + slot;
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

			int rawSlot = getCurrentPage() * 9 * 6 + slot;
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
			shop.applyDefaultTemplate(shop.getDefaultTemplate(), getCurrentPage());
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
		for (ShopEntry entry : shop.getEntries(getCurrentPage())) {
			setItem(entry.getSlot() % (9 * 6), ItemStackUtils.createEntryItemStack(entry, Customer.wrap(player)));
		}
		insertTemplateOverlay();
		refresh(IntStream.range(0, getRows() * 9).toArray());
	}

	private void insertTemplateOverlay() {
		if (shop.getEntries(getCurrentPage()).size() == 0) {
			EntryTemplate template = shop.getDefaultTemplate();
			if (template != null) {
				for (Map.Entry<Integer, ShopEntry> entry : template.getEntries(shop.getRows()).entrySet()) {
					ItemStack actualSlot = getItemStack(entry.getKey());
					if (actualSlot == null || actualSlot.getType() == Material.AIR) {
						ItemStack newItem = entry.getValue().getDisplayItem();
						NBTItem nbtItem = new NBTItem(newItem);
						nbtItem.setBoolean(IGNORE_TAG_KEY, true);
						nbtItem.setInteger(SLOT_TAG_KEY, entry.getKey() + getCurrentPage() * (9 * 6));
						setItem(entry.getKey(), nbtItem.getItem());
					}
				}
			}
		}
	}

	public Menu newTemplatesListMenu() {
		ListEditorMenu<EntryTemplate> menu = new ListEditorMenu<>(Message.GUI_TEMPLATES_CHOOSE, 3, TemplateHandler.getInstance());
		menu.setClickHandler(Action.RIGHT, c -> {
			c.getTarget().setDiscIndex((short) ((c.getTarget().getDiscIndex() + 1) % TemplateHandler.DISCS.length));
			c.getMenu().refresh(c.getSlot());
		});
		menu.setClickHandler(Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), newTemplateApplyMenu(c.getPlayer(), c.getTarget())));
		return menu;
	}

	public Menu newTemplateApplyMenu(Player player, EntryTemplate template) {
		RectInventoryMenu menu = new RectInventoryMenu(Message.GUI_TEMPLATES_APPLY.asComponent(TagResolver.resolver("name", Tag.inserting(template.getName()))), shop.getRows());
		BottomInventoryMenu bottomMenu = new BottomInventoryMenu(InventoryRow.FIRST_ROW);

		bottomMenu.addPreset(MenuPresets.fill(Icon.EMPTY_DARK.create()));

		int dif = getCurrentPage() * 9 * 6;
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
				.withItemStack(Icon.ACCEPT_RP.create()) //TODO conflict with underlying menu
				.withClickHandler(Action.LEFT, clickContext -> {
					shop.applyTemplate(template, getCurrentPage());
					open(clickContext.getPlayer());
				}));
		bottomMenu.setButton(9 + 6, Button.builder()
				.withItemStack(Icon.DECLINE_RP.create())
				.withClickHandler(Action.LEFT, clickContext -> open(player)));
		bottomMenu.setItem(9 + 5, Icon.EMPTY_DARK_RP.create());

		menu.setOpenHandler(o -> bottomMenu.open(o.getPlayer()));
		menu.setCloseHandler(c -> bottomMenu.close(c.getPlayer()));

		return menu;
	}
}
