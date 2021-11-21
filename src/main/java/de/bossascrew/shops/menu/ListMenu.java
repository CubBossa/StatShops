package de.bossascrew.shops.menu;

import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.contexts.BackContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.menu.contexts.TargetContext;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

import java.util.function.Predicate;

@Getter
public class ListMenu<L extends ListMenuElement> extends PagedChestMenu {

	private final ListMenuElementHolder<L> elementHolder;
	@Setter
	private ContextConsumer<TargetContext<ClickType, L>> clickHandler = null;
	@Setter
	private Predicate<L> glowPredicate = listMenuElement -> false;

	public ListMenu(int rowCount, ListMenuElementHolder<L> elementHolder, Message title, ContextConsumer<BackContext> backHandler) {
		super(title.getTranslation(), rowCount, null, null, backHandler);
		this.elementHolder = elementHolder;
	}

	@Override
	public void openInventory(Player player, int page) {
		prepareInventory();
		super.openInventory(player, page);
	}

	private void prepareInventory() {
		super.clearMenuEntries();
		for (L element : elementHolder.getValues()) {
			addMenuEntry(glowPredicate.test(element) ? ItemStackUtils.setGlow(element.getListDisplayItem()) : element.getListDisplayItem(), clickContext -> {

				if (clickHandler != null) {
					clickHandler.accept(new TargetContext<>(clickContext.getPlayer(), clickContext.getItemStack(), clickContext.getSlot(),
							clickContext.getAction(), element));
				}
			});
		}
	}
}