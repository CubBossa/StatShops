package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.CustomerHandler;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.menu.contexts.TargetContext;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.general.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

@Getter
public class ListManagerMenu<L extends ListMenuElement> extends PagedChestMenu {

	private final ListManagementMenuElementHolder<L> elementHolder;
	@Setter
	private ContextConsumer<TargetContext<ClickType, L>> leftClickHandler = null;
	@Setter
	private boolean requireDeleteConfirmation;

	private final Message alreadyEdited;
	private final Message newItemName;
	private final Message newItemLore;
	private final Message confirmDeleteTitle;
	private final Message confirmNewTitle;

	public ListManagerMenu(int rowCount, ListManagementMenuElementHolder<L> elementHolder, boolean requireDeleteConfirmation,
						   Message title, Message alreadyEdited, Message newItemName, Message newItemLore, Message confirmDeleteTitle, Message confirmNewTitle,
						   ContextConsumer<BackContext> backHandler) {
		super(title.getTranslation(), rowCount, null, null, backHandler);
		this.elementHolder = elementHolder;
		this.requireDeleteConfirmation = requireDeleteConfirmation;

		this.alreadyEdited = alreadyEdited;
		this.newItemLore = newItemLore;
		this.newItemName = newItemName;
		this.confirmDeleteTitle = confirmDeleteTitle;
		this.confirmNewTitle = confirmNewTitle;
	}

	@Override
	public void openInventory(Player player, int page) {
		prepareInventory();
		super.openInventory(player, page);
	}

	private void prepareInventory() {
		super.clearMenuEntries();

		setNavigationEntry(4, ItemStackUtils.createItemStack(Material.PAPER, Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE),clickContext -> {});

		for (L element : elementHolder.getValues()) {
			addMenuEntry(element.getListDisplayItem(), clickContext -> {
				Player player = clickContext.getPlayer();

				//Check if interacted object can be edited. Cancel interaction if someone is editing (e.g. prevent delete while someone is editing)
				if (element instanceof Editable editable) {
					if (editable.getEditor() != null) {
						String name = "unknown";
						if (editable.getEditor() instanceof Player p) {
							name = p.getName();
						}
						CustomerHandler.getInstance().getCustomer(clickContext.getPlayer()).sendMessage(alreadyEdited.getKey(), alreadyEdited.getTranslation(
								Template.of("player", name)));
						return;
					}
				}
				if (clickContext.getAction().equals(ClickType.RIGHT)) {
					//Right click = delete
					if (requireDeleteConfirmation) {
						ConfirmMenu confirmMenu = new ConfirmMenu(confirmDeleteTitle.getTranslation(
								Template.of("name", element.getName())), backContext -> openInventory(player, getCurrentPage()));
						confirmMenu.setAcceptHandler(clickContext1 -> {
							elementHolder.delete(element);
							openInventory(player, getCurrentPage());
						});
						confirmMenu.setDenyHandler(clickContext1 -> openInventory(player, 0));
						confirmMenu.openInventory(player);
					} else {
						elementHolder.delete(element);
						openInventory(player, getCurrentPage());
					}

				} else if (clickContext.getAction().equals(ClickType.MIDDLE)) {
					//Middle click = create duplicate
					elementHolder.createDuplicate(element);
					openInventory(player, getCurrentPage());

				} else {
					//Left click = open edit menu for element
					if (leftClickHandler != null) {
						leftClickHandler.accept(new TargetContext<>(clickContext.getPlayer(), clickContext.getItemStack(), clickContext.getSlot(),
								clickContext.getAction(), element));
					}
				}
			});
		}

		setNavigationEntry(7, ItemStackUtils.createItemStack(Material.EMERALD, newItemName, newItemLore), clickContext -> {
			Player player = clickContext.getPlayer();
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(StatShops.getInstance())
					.text("name")
					.title(confirmNewTitle.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(player, currentPage), 1L))
					.onComplete((p, s) -> {
						elementHolder.createNew(s);
						openInventory(player, currentPage);
						return AnvilGUI.Response.close();
					}).open(player);
		});
	}
}
