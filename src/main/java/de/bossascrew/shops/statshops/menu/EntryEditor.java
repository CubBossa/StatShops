package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.menu.ChestMenu;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.general.menu.ListMenu;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.function.Consumer;

public class EntryEditor extends ChestMenu {

	private final ShopEntry entry;
	private final ContextConsumer<BackContext> backHandler;
	@Getter
	@Setter
	@Nullable private Collection<Class<?>> allowedModuleTypes = null;

	public EntryEditor(ShopEntry entry, ContextConsumer<BackContext> backHandler) {
		super(Message.GUI_SHOP_ENTRY, 3);
		this.entry = entry;
		this.backHandler = backHandler;
	}

	private void prepareMenu() {
		fillMenu(null, DefaultSpecialItem.EMPTY_LIGHT);
		setBackHandlerAction(backHandler);
		//Set deco lore
		setItemAndClickHandler(0, 0, ItemStackUtils.createItemStack(entry.getDisplayItem().getType(),
				Message.GUI_ENTRY_SET_LORE_NAME, Message.GUI_ENTRY_SET_LORE_LORE), clickContext -> {

		});
		//Set permissions
		setItemAndClickHandler(1, 0, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.GUI_ENTRY_SET_PERMISSION_NAME.getTranslation(), Message.GUI_ENTRY_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", entry.getPermission() == null ? "X" : entry.getPermission())
				)), clickContext -> {
			Player player = clickContext.getPlayer();
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(StatShops.getInstance())
					.text("shops.item.")
					.title(Message.GUI_ENTRY_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(p), 1L))
					.onComplete((p, s) -> {
						entry.setPermission(s);
						entry.saveToDatabase();
						openInventory(player);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set tags
		setItemAndClickHandler(2, 0, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.GUI_ENTRY_SET_TAGS_NAME, Message.GUI_LIMIT_SET_TAGS_LORE),
				clickContext -> {
					Player player = clickContext.getPlayer();
					TagsEditorMenu<ShopEntry> menu = new TagsEditorMenu<>(entry, Message.GUI_TAGS_TITLE.getTranslation(),
							Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
							Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openInventory(player));
					menu.openInventory(player);
				});

		setItemAndClickHandler(1, 3, ItemStackUtils.createItemStack(entry.getModule() == null ? new ItemStack(Material.BLACK_STAINED_GLASS) :
						entry.getModule().getDisplayItem(), Message.GUI_ENTRY_SET_FUNCTION_NAME.getTranslation(Template.of("name", entry.getModule() == null ?
						Message.GUI_ENTRY_FUNCTION_STATIC_NAME.getTranslation() : entry.getModule().getDisplayName())),
				Message.GUI_ENTRY_SET_FUNCTION_LORE.getTranslations(Template.of("function", entry.getModule() == null ?
						Message.GUI_ENTRY_FUNCTION_STATIC_NAME.getTranslation() : entry.getModule().getDisplayName()))), clickContext -> {

			ListMenu<EntryModuleHandler.EntryModuleProvider> listMenu = new ListMenu<>(3, EntryModuleHandler.getInstance(),
					Message.GUI_ENTRY_SET_FUNCTION_TITLE, backContext -> openInventory(clickContext.getPlayer()));

			listMenu.setDisplayPredicate(provider -> allowedModuleTypes == null || allowedModuleTypes.stream().anyMatch(aClass -> aClass.isInstance(provider)));

			listMenu.setClickHandler(cc -> {
				entry.setModule(cc.getTarget().getModule(entry));
				openInventory(cc.getPlayer());
			});
			listMenu.openInventory(clickContext.getPlayer());
		});

		if (entry.getModule() != null) {
			for (EntryModule.DataSlot<?> dataSlot : entry.getModule().getDataSlots()) {
				Object data = dataSlot.getData();
				if (dataSlot.getType() == ItemStack.class) {

				} else if (dataSlot.getType() == String.class) {

				} else if (dataSlot.getType() == Integer.class) {

				}
			}
		}

		//TODO alle dataslots für das Modul
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareMenu();
		return super.openInventorySync(player, inventoryPreparer);
	}
}
