package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.menu.contexts.TargetContext;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.CustomerHandler;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.jetbrains.annotations.Nullable;

@Getter
public class ListEditorMenu<L extends ListMenuElement> extends PagedChestMenu {

	private final ListEditorMenuElementHolder<L> elementHolder;
	@Setter
	private ContextConsumer<TargetContext<ClickType, L>> leftClickHandler = null;
	@Setter
	private boolean requireDeleteConfirmation;

	private final Message alreadyEdited;
	private final Message newItemName;
	private final Message newItemLore;
	private final Message confirmDeleteTitle;
	private final Message confirmNewTitle;
	private Message selectTypeTitle;

	public ListEditorMenu(int rowCount, ListEditorMenuElementHolder<L> elementHolder, boolean requireDeleteConfirmation,
						  Message title, Message alreadyEdited, Message newItemName, Message newItemLore, Message confirmDeleteTitle, Message confirmNewTitle,
						  ContextConsumer<BackContext> backHandler) {
		this(rowCount, elementHolder, requireDeleteConfirmation, title, alreadyEdited, newItemName, newItemLore, confirmDeleteTitle, confirmNewTitle, Message.NONE, backHandler);
	}

	public ListEditorMenu(int rowCount, ListEditorMenuElementHolder<L> elementHolder, boolean requireDeleteConfirmation,
						  Message title, Message alreadyEdited, Message newItemName, Message newItemLore, Message confirmDeleteTitle, Message confirmNewTitle,
						  Message selectTypeTitle, ContextConsumer<BackContext> backHandler) {
		super(title.getTranslation(), rowCount, null, null, backHandler);
		this.elementHolder = elementHolder;
		this.requireDeleteConfirmation = requireDeleteConfirmation;

		this.alreadyEdited = alreadyEdited;
		this.newItemLore = newItemLore;
		this.newItemName = newItemName;
		this.confirmDeleteTitle = confirmDeleteTitle;
		this.confirmNewTitle = confirmNewTitle;
		this.selectTypeTitle = selectTypeTitle;
	}

	@Override
	public void openInventory(Player player, int page) {
		prepareInventory();
		super.openInventory(player, page);
	}

	private void prepareInventory() {
		super.clearMenuEntries();

		setNavigationEntry(4, ItemStackUtils.createInfoItem(Message.GENERAL_GUI_LIST_INFO_NAME, Message.GENERAL_GUI_LIST_INFO_LORE),clickContext -> {});

		for (L element : elementHolder.getValues()) {
			addMenuEntry(element.getListDisplayItem(), clickContext -> {
				Player player = clickContext.getPlayer();

				//Check if interacted object can be edited. Cancel interaction if someone is editing (e.g. prevent delete while someone is editing)
				if (element instanceof Editable editable) {
					if (editable.getEditor() != null && !(editable.getEditor() instanceof Player p && p.getUniqueId().equals(player.getUniqueId()))) {
						String name = "unknown";
						if (editable.getEditor() instanceof Player p) {
							name = p.getName();
						}
						CustomerHandler.getInstance().getCustomer(clickContext.getPlayer()).sendMessage(alreadyEdited.getKey(), alreadyEdited.getTranslation(
								TagResolver.resolver("player", Tag.inserting(Component.text(name)))));
						return;
					}
				}
				if (clickContext.getAction().equals(ClickType.RIGHT)) {
					//Right click = delete
					if (requireDeleteConfirmation) {
						ConfirmMenu confirmMenu = new ConfirmMenu(confirmDeleteTitle.getTranslation(
								TagResolver.resolver("name", Tag.inserting(element.getName()))), backContext -> openInventory(player, getCurrentPage()));
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
			if (elementHolder instanceof ListEditorMenuTypedElementHolder<L> typedHolder) {
				openTypeInput(player, typedHolder);
			} else {
				openNameInput(player, null);
			}
		});
	}

	public void openTypeInput(Player player, ListEditorMenuTypedElementHolder<L> holder) {

		int size = holder.getTypes().size();
		if (size == 0) {
			openNameInput(player, null);
		} else if (size == 1) {
			openNameInput(player, holder.getTypes().get(0).type());
		}

		LMenu<ListEditorMenuTypedElementHolder.Provider<L>> menu = new LMenu<>(3, holder::getTypes,
				selectTypeTitle, backContext -> openInventory(backContext.getPlayer(), currentPage));

		menu.setClickHandler(clickContext -> openNameInput(clickContext.getPlayer(), clickContext.getTarget().type()));

		menu.openInventory(player, currentPage);
	}

	public void openNameInput(Player player, @Nullable Class<? extends L> type) {
		player.closeInventory();
		new AnvilGUI.Builder()
				.plugin(StatShops.getInstance())
				.text("name")
				.title(confirmNewTitle.getLegacyTranslation())
				.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(player, currentPage), 1L))
				.onComplete((p, s) -> {
					if (elementHolder instanceof ListEditorMenuTypedElementHolder typedElementHolder) {
						typedElementHolder.createNew(s, type);
					} else {
						elementHolder.createNew(s);
					}

					openInventory(player, currentPage);
					return AnvilGUI.Response.close();
				}).open(player);
	}
}
