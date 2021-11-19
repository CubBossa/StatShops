package de.bossascrew.shops.menu;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.shop.Taggable;
import de.bossascrew.shops.util.ItemStackUtils;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;

import java.util.ArrayList;

public class TagsEditorMenu extends PagedChestMenu {

	private final Taggable taggable;

	private final Message newTagTitle;
	private final Message newTagName;
	private final Message newTagLore;
	private final Message confirmRemove;

	public TagsEditorMenu(Taggable taggable, Component title,
						  Message newTagTitle, Message newTagName, Message newTagLore, Message confirmRemove,
						  ContextConsumer<BackContext> backHandler) {
		super(title, 3, null, closeContext -> {
			if (taggable instanceof DatabaseObject databaseObject) {
				databaseObject.saveToDatabase();
			}
		}, backHandler);
		this.taggable = taggable;
		this.newTagTitle = newTagTitle;
		this.newTagName = newTagName;
		this.newTagLore = newTagLore;
		this.confirmRemove = confirmRemove;
	}

	public void prepareMenu() {
		clearMenuEntries();

		setNavigationEntry(4, ItemStackUtils.createItemStack(Material.PAPER, Message.GENERAL_GUI_TAGS_INFO_NAME, Message.GENERAL_GUI_TAGS_INFO_LORE),clickContext -> {});

		setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD, newTagName, newTagLore), clickContext -> {
			Player player = clickContext.getPlayer();

			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(ShopPlugin.getInstance())
					.text("tag-me")
					.title(newTagTitle.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(ShopPlugin.getInstance(), () -> openInventory(player, getCurrentPage()), 1L))
					.onComplete((p, s) -> {
						taggable.addTag(s);
						openInventory(player, getPageCount() - 1);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		for (String tag : taggable.getTags()) {
			addMenuEntry(ItemStackUtils.createItemStack(Material.NAME_TAG, Component.text(tag, NamedTextColor.WHITE), new ArrayList<>()), clickContext -> {
				if (clickContext.getAction().isRightClick()) {
					if (ShopPlugin.getInstance().getShopsConfig().isConfirmTagDeletion()) {
						ConfirmMenu confirmMenu = new ConfirmMenu(confirmRemove.getTranslation(Template.of("name", tag)));
						confirmMenu.setDenyHandler(c -> openInventory(c.getPlayer(), getCurrentPage()));
						confirmMenu.setCloseHandler(c -> openInventory(c.getPlayer(), getCurrentPage()));
						confirmMenu.setAcceptHandler(c -> {
							taggable.removeTag(tag);
							openInventory(c.getPlayer(), getCurrentPage());
						});
						confirmMenu.openInventory(clickContext.getPlayer());
					} else {
						taggable.removeTag(tag);
						openInventory(clickContext.getPlayer(), getCurrentPage());
					}
				}
			});
		}
	}

	@Override
	public void openInventory(Player player, int page) {
		prepareMenu();
		super.openInventory(player, page);
	}
}
