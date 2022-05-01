package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.guiframework.inventory.Action;
import de.cubbossa.guiframework.inventory.Button;
import de.cubbossa.guiframework.inventory.context.ClickContext;
import de.cubbossa.guiframework.inventory.context.ContextConsumer;
import de.cubbossa.guiframework.inventory.context.TargetContext;
import de.cubbossa.guiframework.inventory.implementations.AnvilMenu;
import de.cubbossa.guiframework.inventory.implementations.ListMenu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;

@Getter
@Setter
public abstract class DataSlot<T> implements ConfigurationSerializable {

	public static final Map<String, Pair<Message, Message>> DATA_MESSAGE_MAP = new HashMap<>();

	static {
		DATA_MESSAGE_MAP.put("sell_pay_price_amount", new Pair<>(Message.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_LORE));
		DATA_MESSAGE_MAP.put("buy_pay_price_amount", new Pair<>(Message.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_LORE));
		DATA_MESSAGE_MAP.put("buy_pay_price_equation", new Pair<>(Message.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_LORE));
		DATA_MESSAGE_MAP.put("sell_pay_price_equation", new Pair<>(Message.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_LORE));
		DATA_MESSAGE_MAP.put("buy_pay_price_item", new Pair<>(Message.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_LORE));
		DATA_MESSAGE_MAP.put("sell_pay_price_item", new Pair<>(Message.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_LORE));
		DATA_MESSAGE_MAP.put("pagination_page", new Pair<>(Message.GUI_ENTRY_FUNCTION_PAGE_NAME, Message.GUI_ENTRY_FUNCTION_PAGE_LORE));
		DATA_MESSAGE_MAP.put("pagination_mode", new Pair<>(Message.NONE, Message.NONE));
		DATA_MESSAGE_MAP.put("open_shop", new Pair<>(Message.GUI_ENTRY_FUNCTION_OPENED_SHOP_NAME, Message.GUI_ENTRY_FUNCTION_OPENED_SHOP_LORE));
		DATA_MESSAGE_MAP.put("purchasable", new Pair<>(Message.GUI_ENTRY_FUNCTION_PURCHASABLE_NAME, Message.GUI_ENTRY_FUNCTION_PURCHASABLE_LORE));
		DATA_MESSAGE_MAP.put("sellable", new Pair<>(Message.GUI_ENTRY_FUNCTION_SELLABLE_NAME, Message.GUI_ENTRY_FUNCTION_SELLABLE_LORE));
		DATA_MESSAGE_MAP.put("purchasable_stacked", new Pair<>(Message.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_NAME, Message.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_LORE));
		DATA_MESSAGE_MAP.put("sellable_stacked", new Pair<>(Message.GUI_ENTRY_FUNCTION_SELLABLE_STACKED_NAME, Message.GUI_ENTRY_FUNCTION_SELLABLE_STACKED_LORE));
		DATA_MESSAGE_MAP.put("gain_price_item", new Pair<>(Message.GUI_ENTRY_FUNCTION_GAIN_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_GAIN_ITEM_LORE));
		DATA_MESSAGE_MAP.put("gain_price_amount", new Pair<>(Message.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_LORE));
		DATA_MESSAGE_MAP.put("command", new Pair<>(Message.GUI_ENTRY_FUNCTION_COMMAND_NAME, Message.GUI_ENTRY_FUNCTION_COMMAND_LORE));
	}


	private ItemStack displayItem = new ItemStack(Material.BARRIER);
	private final Message typeMessage;
	private Message name = null;
	private Message lore = null;
	private @Nullable
	T data;
	private Consumer<T> updateHandler = t -> {
	};
	private Function<T, Component> dataFormatter = t -> Component.text("" + t);

	public DataSlot(Message typeMessage) {
		this.typeMessage = typeMessage;
	}

	public abstract Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler();

	public void runUpdateHandler() {
		updateHandler.accept(data);
	}

	public void setUpdateHandler(Consumer<T> updateHandler) {
		this.updateHandler = updateHandler;
		runUpdateHandler();
	}

	public void setData(T data) {
		this.data = data;
		updateHandler.accept(data);
	}

	public ItemStack getDisplayItem() {
		if (name == null || lore == null) {
			return null;
		}
		return ItemStackUtils.setNameAndLore(displayItem, typeMessage.getTranslation(TagResolver.resolver("name", Tag.inserting(name))),
				lore.getTranslations(TagResolver.resolver("current", Tag.inserting(dataFormatter.apply(data)))));
	}

	public static class TextSlot extends DataSlot<String> {

		public TextSlot(String data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_STRING);
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
				AnvilMenu menu = new AnvilMenu(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_STRING.getTranslation(TagResolver.resolver("name", Tag.inserting(getName()))),
						"" + getData());
				menu.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
					setData(s.getTarget());
					s.getPlayer().closeInventory();
					c.getMenu().refresh(c.getSlot());
				});
				return menu;
			}));
		}

		@NotNull
		@Override
		public Map<String, Object> serialize() {
			return Map.of("data", getData());
		}

		public static TextSlot deserialize(Map<String, Object> values) {
			return new TextSlot((String) values.get("data"));
		}
	}

	public static class EquationSlot extends DataSlot<String> {


		public EquationSlot(String data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION);
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
				AnvilMenu menu = new AnvilMenu(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION.getTranslation(TagResolver.resolver("name", Tag.inserting(getName()))),
						"" + getData());
				menu.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
					setData(s.getTarget());
					s.getPlayer().closeInventory();
					c.getMenu().refresh(c.getSlot());
				});
				return menu;
			}));
		}

		@NotNull
		@Override
		public Map<String, Object> serialize() {
			return Map.of("data", getData());
		}

		public static EquationSlot deserialize(Map<String, Object> values) {
			return new EquationSlot((String) values.get("data"));

		}
	}

	public static class ItemStackSlot extends DataSlot<ItemStack> {

		public ItemStackSlot(ItemStack data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK);
			super.setData(data);
			super.setDisplayItem(data.clone());
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, c -> {
				ItemStack hand = c.getPlayer().getItemOnCursor();
				if (hand != null && !hand.getType().equals(Material.AIR)) {
					setData(hand.clone());
					getData().setAmount(1);
					setDisplayItem(getData());
					c.getMenu().refresh(c.getSlot());
				}
			});
		}

		@NotNull
		@Override
		public Map<String, Object> serialize() {
			Map<String, Object> map = new HashMap<>();
			map.put("data", getData());
			return map;
		}

		public static ItemStackSlot deserialize(Map<String, Object> values) {
			return new ItemStackSlot((ItemStack) values.get("data"));
		}
	}

	public static class BooleanSlot extends DataSlot<Boolean> {

		public BooleanSlot(boolean data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL);
			setData(data);
			super.setDisplayItem(ItemStackUtils.createButtonItemStack(data, Message.NONE, Message.NONE));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, c -> {
				setData(Boolean.FALSE.equals(getData()));
				setDisplayItem(ItemStackUtils.createButtonItemStack(Boolean.TRUE.equals(getData()), Message.NONE, Message.NONE));
				c.getMenu().refresh(c.getSlot());
			});
		}

		@NotNull
		@Override
		public Map<String, Object> serialize() {
			Map<String, Object> map = new HashMap<>();
			map.put("data", getData());
			return map;
		}

		public static BooleanSlot deserialize(Map<String, Object> values) {
			return new BooleanSlot((Boolean) values.get("data"));

		}
	}

	public static class NumberSlot extends DataSlot<Double> {

		public NumberSlot(Double data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER);
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, data.intValue()), 64)));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
				AnvilMenu menu = new AnvilMenu(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.getTranslation(TagResolver.resolver("name", Tag.inserting(getName()))),
						"" + getData());
				menu.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
					try {
						setData(Double.parseDouble(s.getTarget().replace(" ", "")));
						super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, getData().intValue()), 64)));
						s.getPlayer().closeInventory();
						c.getMenu().refresh(c.getSlot());
					} catch (NumberFormatException e) {
						c.getPlayer().playSound(c.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
					}
				});
				return menu;
			}));
		}

		public void setData(int data) {
			super.setData((double) data);
		}

		@NotNull
		@Override
		public Map<String, Object> serialize() {
			Map<String, Object> map = new HashMap<>();
			map.put("data", getData());
			return map;
		}

		public static NumberSlot deserialize(Map<String, Object> values) {
			return new NumberSlot((Double) values.get("data"));

		}
	}

	public static class ShopSlot extends DataSlot<UUID> {

		Function<UUID, ItemStack> displayItem = uuid -> {
			Shop shop = uuid == null ? null : ShopHandler.getInstance().getShop(uuid);
			return shop == null ? new ItemStack(Material.VILLAGER_SPAWN_EGG) : ShopHandler.getInstance().getDisplayItem(shop);
		};

		public ShopSlot(UUID uuid) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_SHOP);
			super.setData(uuid);
			super.setDataFormatter(aUuid -> aUuid == null ? Component.text("none", NamedTextColor.GRAY) : ShopHandler.getInstance().getShop(aUuid).getName());
			super.setDisplayItem(displayItem.apply(uuid));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
				int shops = ShopHandler.getInstance().getShops().size();
				ListMenu menu = new ListMenu(Message.GUI_SHOPS_TITLE, Integer.max(3, Integer.min(shops % 9, 6)));
				for (Shop shop : ShopHandler.getInstance().getShops()) {
					menu.addListEntry(Button.builder()
							.withItemStack(() -> Objects.equals(shop.getUUID(), getData()) ?
									ItemStackUtils.setGlow(ShopHandler.getInstance().getDisplayItem(shop)) : ShopHandler.getInstance().getDisplayItem(shop))
							.withClickHandler(Action.LEFT, cl -> {
								setData(shop.getUUID());
								setDisplayItem(ShopHandler.getInstance().getDisplayItem(shop).clone());
								menu.refresh(menu.getListSlots());
							}));
				}
				return menu;
			}));
		}

		@Override
		public ItemStack getDisplayItem() {
			setDisplayItem(displayItem.apply(getData()));
			return super.getDisplayItem();
		}

		@NotNull
		@Override
		public Map<String, Object> serialize() {
			Map<String, Object> map = new HashMap<>();
			map.put("shop", getData() == null ? null : getData().toString());
			return map;
		}

		public static ShopSlot deserialize(Map<String, Object> values) {
			String uuidString = (String) values.get("shop");
			if (uuidString == null) {
				System.out.println("invalid string: " + uuidString);
				return null;
			}
			try {
				return new ShopSlot(UUID.fromString(uuidString));
			} catch (Exception ignored) {
				System.out.println("invalid string: " + uuidString);
				return null;
			}
		}
	}
}