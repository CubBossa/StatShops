package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Allows to be used as the list element holder for a ListMenu instance.
 * The element holder provides all elements to be listed in the menu
 *
 * @param <L> The element that will be listed in the ListMenu instance.
 */
public interface ListEditorMenuTypedElementHolder<L extends ListMenuElement> extends ListEditorMenuElementHolder<L> {

	/**
	 * @param input the input string from the anvil gui
	 * @return A new object with default values
	 */
	<T extends L> T createNew(String input, @Nullable Class<T> type);

	/**
	 * @return a list of all allowed types for this element.
	 */
	List<Provider<L>> getTypes();

	record Provider<L>(Class<? extends L> type, Material material, Message name,
					   Message lore) implements ListMenuElement {
		@Override
		public Component getName() {
			return name.getTranslation();
		}

		@Override
		public ItemStack getListDisplayItem() {
			return ItemStackUtils.createItemStack(material, name, lore);
		}
	}
}
