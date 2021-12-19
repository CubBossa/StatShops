package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.menu.contexts.ClickContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.data.Message;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;

@RequiredArgsConstructor
@Getter
public abstract class DataSlot<T> {

	@Setter
	private ItemStack displayItem = new ItemStack(Material.BARRIER); //TODO raus hier!
	private final Component name;
	private final List<Component> lore;
	@Setter
	private ContextConsumer<ClickContext> clickHandler = c -> {
	};
	@Setter
	private @Nullable T data;

	public ItemStack getDisplayItem() {
		return ItemStackUtils.createItemStack(displayItem, name, lore);
	}

	public static class EquationSlot extends DataSlot<String> {

		public EquationSlot(Component name, List<Component> lore) {
			super(name, lore); //TODO trademodule in erben aufspalten und
		}
	}

	public static class ItemStackSlot extends DataSlot<ItemStack> {

		public ItemStackSlot(ItemStack data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK.getTranslation(),
					Message.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK_DESC.getTranslations(Template.of("current", TextUtils.toComponent(data))));
			super.setClickHandler(clickContext -> {
				ItemStack hand = clickContext.getPlayer().getItemOnCursor();
				if (hand != null && !hand.getType().equals(Material.AIR)) {
					setData(data);
				}
			});
			super.setData(data);
			super.setDisplayItem(data);
		}
	}

	public static class IntegerSlot extends DataSlot<Integer> {

		public IntegerSlot(Integer data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.getTranslation(),
					Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER_DESC.getTranslations(Template.of("current", Component.text(data))));
			super.setClickHandler(clickContext -> {
				clickContext.getPlayer().sendMessage("Hier öffnet sich ein anvil gui");
			});
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, data), 64)));
		}
	}
}