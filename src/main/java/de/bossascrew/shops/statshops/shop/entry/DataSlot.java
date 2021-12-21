package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.menu.contexts.TargetContext;
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
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.function.Consumer;

@RequiredArgsConstructor
@Getter
@Setter
public abstract class DataSlot<T> {

	private final String storageKey;
	private ItemStack displayItem = new ItemStack(Material.BARRIER);
	private final Message typeMessage;
	private Message name;
	private Message lore;
	private ContextConsumer<TargetContext<ClickType, Runnable>> clickHandler = c -> {
	};
	private @Nullable T data;
	private Consumer<T> updateHandler = t -> {
	};

	public DataSlot(String storageKey, Message typeMessage, Message name, Message lore) {
		this.storageKey = storageKey;
		this.typeMessage = typeMessage;
		this.name = name;
		this.lore = lore;
	}

	public void setData(T data) {
		this.data = data;
		updateHandler.accept(data);
	}

	public ItemStack getDisplayItem() {
		return ItemStackUtils.setNameAndLore(displayItem, typeMessage.getTranslation(Template.of("name", name.getTranslation())),
				lore.getTranslations(Template.of("current", "" + data)));
	}

	public static class EquationSlot extends DataSlot<String> {

		public EquationSlot(String storageKey, String data, Message name, Message lore) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION, name, lore);
			super.setClickHandler(clickContext -> {
				//TODO
			});
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
		}
	}

	public static class ItemStackSlot extends DataSlot<ItemStack> {

		public ItemStackSlot(String storageKey, ItemStack data, Message name, Message lore) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK, name, lore);
			super.setClickHandler(clickContext -> {
				ItemStack hand = clickContext.getPlayer().getItemOnCursor();
				if (hand != null && !hand.getType().equals(Material.AIR)) {
					setData(hand.clone());
					getData().setAmount(1);
					setDisplayItem(getData());
				}
			});
			super.setData(data);
			super.setDisplayItem(data.clone());
		}
	}

	public static class BooleanSlot extends DataSlot<Boolean> {

		public BooleanSlot(String storageKey, boolean data, Message name, Message lore) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL, name, lore);
			setData(data);
			super.setClickHandler(clickContext -> {
				setData(Boolean.FALSE.equals(getData()));
				super.setDisplayItem(ItemStackUtils.createButtonItemStack(Boolean.TRUE.equals(getData()), Message.NONE, Message.NONE));
			});
			super.setDisplayItem(ItemStackUtils.createButtonItemStack(data, Message.NONE, Message.NONE));
		}
	}

	public static class NumberSlot extends DataSlot<Double> {

		public NumberSlot(String storageKey, Double data, Message name, Message lore) {
			super(storageKey, Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER, name, lore);
			super.setClickHandler(clickContext -> {
				Player player = clickContext.getPlayer();
				player.closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("" + getData())
						.title(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.getLegacyTranslation(Template.of("name", name.getTranslation())))
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> clickContext.getTarget().run(), 1L))
						.onComplete((p, s) -> {
							try {
								setData(Double.parseDouble(s.replace(" ", "")));
								clickContext.getTarget().run();
								return AnvilGUI.Response.close();
							} catch (NumberFormatException e) {
								return AnvilGUI.Response.text("" + getData());
							}
						}).open(player);
			});
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, data.intValue()), 64)));
		}

		public void setData(int data) {
			super.setData((double) data);
		}
	}
}