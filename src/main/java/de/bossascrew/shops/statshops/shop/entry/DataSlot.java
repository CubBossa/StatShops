package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.menu.contexts.ClickContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.function.Consumer;

@RequiredArgsConstructor
@Getter
@Setter
public abstract class DataSlot<T> {

	//TODO messages mit lore, damit man zb erklären kann wieso man das erkaufte item seperat setzen kann

	private final String storageKey;
	private ItemStack displayItem = new ItemStack(Material.BARRIER);
	private Component name;
	private List<Component> lore;
	private ContextConsumer<ClickContext> clickHandler = c -> {
	};
	private @Nullable T data;
	private Consumer<T> updateHandler = t -> {
	};

	public DataSlot(String storageKey, Component name, List<Component> lore) {
		this.storageKey = storageKey;
		this.name = name;
		this.lore = lore;
	}

	public void setData(T data) {
		this.data = data;
		updateHandler.accept(data);
	}

	public ItemStack getDisplayItem() {
		return ItemStackUtils.setNameAndLore(displayItem, name, lore);
	}

	public static class EquationSlot extends DataSlot<String> {

		public EquationSlot(String storageKey, Component name, List<Component> lore) {
			super(storageKey, name, lore);
		}
	}

	public static class ItemStackSlot extends DataSlot<ItemStack> {

		public ItemStackSlot(String storageKey, ItemStack data, Message name) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK.getTranslation(Template.of("name", name.getTranslation())),
					Message.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK_DESC.getTranslations(Template.of("current", TextUtils.toComponent(data))));
			super.setClickHandler(clickContext -> {
				ItemStack hand = clickContext.getPlayer().getItemOnCursor();
				if (hand != null && !hand.getType().equals(Material.AIR)) {
					setData(hand);
					setDisplayItem(getData().clone());
				}
			});
			super.setData(data);
			super.setDisplayItem(data.clone());
		}
	}

	public static class BooleanSlot extends DataSlot<Boolean> {

		public BooleanSlot(String storageKey, boolean data, Message name) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL.getTranslation(Template.of("name", name.getTranslation())),
					Message.GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL_DESC.getTranslations(Template.of("current", "" + data)));
			setData(data);
			super.setClickHandler(clickContext -> {
				setData(Boolean.FALSE.equals(getData()));
				super.setDisplayItem(ItemStackUtils.createButtonItemStack(Boolean.TRUE.equals(getData()), Message.NONE, Message.NONE));
				super.setLore(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL_DESC.getTranslations(Template.of("current", "" + Boolean.TRUE.equals(getData()))));
			});
			super.setDisplayItem(ItemStackUtils.createButtonItemStack(data, Message.NONE, Message.NONE));
		}
	}

	public static class IntegerSlot extends DataSlot<Integer> {

		public IntegerSlot(String storageKey, Integer data, Message name) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.getTranslation(Template.of("name", name.getTranslation())),
					Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER_DESC.getTranslations(Template.of("current", Component.text(data))));
			super.setClickHandler(clickContext -> {
				Player player = clickContext.getPlayer();
				player.closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("" + 10)
						.title(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.getLegacyTranslation(Template.of("name", name.getTranslation())))
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> {
							//TODO backhandler
						}, 1L))
						.onComplete((p, s) -> {
							try {
								setData(Integer.parseInt(s));
								//TODO call backhandler
								return AnvilGUI.Response.close();
							} catch (NumberFormatException e) {
								return AnvilGUI.Response.text("Lol" /*TODO*/);
							}
						}).open(player);
			});
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, data), 64)));
		}
	}
}