package de.bossascrew.shops.menu;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.EntryModuleHandler;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;

public class EntryEditor extends ChestMenu {

	ShopEntry entry;

	public EntryEditor(ShopEntry entry, ContextConsumer<BackContext> backHandler) {
		super(Message.MANAGER_GUI_SHOP_ENTRY, 3);
		this.entry = entry;
		setBackHandlerAction(backHandler);
	}

	private void prepareMenu() {
		fillMenu(DefaultSpecialItem.EMPTY_LIGHT);
		//Set deco lore
		setItemAndClickHandler(0, 0, ItemStackUtils.createItemStack(entry.getDisplayItem().getType(),
				Message.MANAGER_GUI_ENTRY_SET_LORE_NAME, Message.MANAGER_GUI_ENTRY_SET_LORE_LORE), clickContext -> {

		});
		//Set permissions
		setItemAndClickHandler(1, 0, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.MANAGER_GUI_ENTRY_SET_PERMISSION_NAME.getTranslation(), Message.MANAGER_GUI_ENTRY_SET_PERMISSION_LORE.getTranslations(
						Template.of("permission", entry.getPermission() == null ? "X" : entry.getPermission())
				)), clickContext -> {
			Player player = clickContext.getPlayer();
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("shops.item.")
					.title(Message.MANAGER_GUI_ENTRY_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openInventory(p), 1L))
					.onComplete((p, s) -> {
						entry.setPermission(s);
						entry.saveToDatabase();
						openInventory(player);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set tags
		setItemAndClickHandler(2, 0, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.MANAGER_GUI_ENTRY_SET_TAGS_NAME, Message.MANAGER_GUI_LIMIT_SET_TAGS_LORE),
				clickContext -> {
					Player player = clickContext.getPlayer();
					TagsEditorMenu menu = new TagsEditorMenu(entry, Message.MANAGER_GUI_TAGS_TITLE.getTranslation(),
							Message.MANAGER_GUI_TAGS_NEW_TAG_TITLE, Message.MANAGER_GUI_TAGS_NEW_TAG_NAME, Message.MANAGER_GUI_TAGS_NEW_TAG_LORE,
							Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openInventory(player));
					menu.openInventory(player);
				});

		//handle items
		//TODO einmal type
		//TODO alle dataslots für das item

		setItemAndClickHandler(1, 3, ItemStackUtils.createItemStack(Material.REPEATER, Message.MANAGER_GUI_ENTRY_SET_FUNCTION_NAME,
				Message.MANAGER_GUI_ENTRY_SET_FUNCTION_LORE), clickContext -> {
			//TODO message
			ListMenu<EntryModuleHandler.EntryModuleProvider> listMenu = new ListMenu<>(3, EntryModuleHandler.getInstance(), null, backContext -> {
				openInventory(clickContext.getPlayer());
			});
			listMenu.setClickHandler(cc -> {
				entry.setModule(cc.getTarget().getModule(entry));
			});
			listMenu.openInventory(clickContext.getPlayer());
		});
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareMenu();
		return super.openInventorySync(player, inventoryPreparer);
	}
}
