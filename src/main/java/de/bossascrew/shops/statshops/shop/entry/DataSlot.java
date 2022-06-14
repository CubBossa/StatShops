package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.menu.ListEditorMenu;
import de.bossascrew.shops.statshops.menu.MainMenu;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.menuframework.GUIHandler;
import de.cubbossa.menuframework.inventory.Action;
import de.cubbossa.menuframework.inventory.Button;
import de.cubbossa.menuframework.inventory.ListMenuSupplier;
import de.cubbossa.menuframework.inventory.TopMenu;
import de.cubbossa.menuframework.inventory.context.ClickContext;
import de.cubbossa.menuframework.inventory.context.ContextConsumer;
import de.cubbossa.menuframework.inventory.context.TargetContext;
import de.cubbossa.menuframework.inventory.implementations.AnvilMenu;
import de.cubbossa.menuframework.inventory.implementations.ListMenu;
import de.cubbossa.translations.Message;
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

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.logging.Level;

@Getter
@Setter
public abstract class DataSlot<T> implements ConfigurationSerializable {

	public static final Map<String, Pair<Message, Message>> DATA_MESSAGE_MAP = new HashMap<>();

	static {
		DATA_MESSAGE_MAP.put("sell_pay_price_amount", new Pair<>(Messages.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_NAME, Messages.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_LORE));
		DATA_MESSAGE_MAP.put("buy_pay_price_amount", new Pair<>(Messages.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_NAME, Messages.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_LORE));
		DATA_MESSAGE_MAP.put("buy_pay_price_equation", new Pair<>(Messages.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_NAME, Messages.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_LORE));
		DATA_MESSAGE_MAP.put("sell_pay_price_equation", new Pair<>(Messages.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_NAME, Messages.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_LORE));
		DATA_MESSAGE_MAP.put("buy_pay_price_item", new Pair<>(Messages.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_NAME, Messages.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_LORE));
		DATA_MESSAGE_MAP.put("sell_pay_price_item", new Pair<>(Messages.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_NAME, Messages.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_LORE));
		DATA_MESSAGE_MAP.put("pagination_page", new Pair<>(Messages.GUI_ENTRY_FUNCTION_PAGE_NAME, Messages.GUI_ENTRY_FUNCTION_PAGE_LORE));
		DATA_MESSAGE_MAP.put("pagination_mode", new Pair<>(Messages.NONE, Messages.NONE));
		DATA_MESSAGE_MAP.put("open_shop", new Pair<>(Messages.GUI_ENTRY_FUNCTION_OPENED_SHOP_NAME, Messages.GUI_ENTRY_FUNCTION_OPENED_SHOP_LORE));
		DATA_MESSAGE_MAP.put("purchasable", new Pair<>(Messages.GUI_ENTRY_FUNCTION_PURCHASABLE_NAME, Messages.GUI_ENTRY_FUNCTION_PURCHASABLE_LORE));
		DATA_MESSAGE_MAP.put("sellable", new Pair<>(Messages.GUI_ENTRY_FUNCTION_SELLABLE_NAME, Messages.GUI_ENTRY_FUNCTION_SELLABLE_LORE));
		DATA_MESSAGE_MAP.put("purchasable_stacked", new Pair<>(Messages.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_NAME, Messages.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_LORE));
		DATA_MESSAGE_MAP.put("sellable_stacked", new Pair<>(Messages.GUI_ENTRY_FUNCTION_SELLABLE_STACKED_NAME, Messages.GUI_ENTRY_FUNCTION_SELLABLE_STACKED_LORE));
		DATA_MESSAGE_MAP.put("gain_price_item", new Pair<>(Messages.GUI_ENTRY_FUNCTION_GAIN_ITEM_NAME, Messages.GUI_ENTRY_FUNCTION_GAIN_ITEM_LORE));
		DATA_MESSAGE_MAP.put("gain_price_amount", new Pair<>(Messages.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_NAME, Messages.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_LORE));
		DATA_MESSAGE_MAP.put("command", new Pair<>(Messages.GUI_ENTRY_FUNCTION_COMMAND_NAME, Messages.GUI_ENTRY_FUNCTION_COMMAND_LORE));
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
		return ItemStackUtils.setNameAndLore(displayItem, typeMessage.asComponent(TagResolver.resolver("name", Tag.inserting(name))),
				lore.asComponents(TagResolver.resolver("current", Tag.inserting(dataFormatter.apply(data)))));
	}

	public static class TextSlot extends DataSlot<String> {

		public TextSlot(String data) {
			super(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_STRING);
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> {
				if(c.getMenu() instanceof TopMenu topMenu) {
					topMenu.openSubMenu(c.getPlayer(), () -> {
						AnvilMenu menu = MainMenu.newAnvilMenu(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_STRING.asComponent(TagResolver.resolver("name", Tag.inserting(getName()))),
								"" + getData());
						menu.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
							setData(s.getTarget());
							menu.openPreviousMenu(s.getPlayer());
							c.getMenu().refresh(c.getSlot());
						});
						return menu;
					});
				} else {
					GUIHandler.getInstance().getLogger().log(Level.SEVERE, "TopMenu required for DataSlot click handler.");
				}
			});
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
			super(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION);
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> {
				if(c.getMenu() instanceof TopMenu topMenu) {
					topMenu.openSubMenu(c.getPlayer(), () -> {
						AnvilMenu menu = MainMenu.newAnvilMenu(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION.asComponent(TagResolver.resolver("name", Tag.inserting(getName()))),
								"" + getData());
						menu.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
							setData(s.getTarget());
							menu.openPreviousMenu(s.getPlayer());
							c.getMenu().refresh(c.getSlot());
						});
						return menu;
					});
				} else {
					GUIHandler.getInstance().getLogger().log(Level.SEVERE, "TopMenu required for DataSlot click handler.");
				}
			});
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
			super(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK);
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
			super(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL);
			setData(data);
			super.setDisplayItem(ItemStackUtils.createButtonItemStack(data, Messages.NONE, Messages.NONE));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, c -> {
				setData(Boolean.FALSE.equals(getData()));
				setDisplayItem(ItemStackUtils.createButtonItemStack(Boolean.TRUE.equals(getData()), Messages.NONE, Messages.NONE));
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
			super(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER);
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, data.intValue()), 64)));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> {
				if(c.getMenu() instanceof TopMenu topMenu) {
					topMenu.openSubMenu(c.getPlayer(), () -> {
						AnvilMenu menu = MainMenu.newAnvilMenu(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.asComponent(TagResolver.resolver("name", Tag.inserting(getName()))),
								"" + getData());
						menu.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
							try {
								setData(Double.parseDouble(s.getTarget().replace(" ", "")));
								super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, getData().intValue()), 64)));
								menu.openPreviousMenu(s.getPlayer());
								c.getMenu().refresh(c.getSlot());
							} catch (NumberFormatException e) {
								c.getPlayer().playSound(c.getPlayer().getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
							}
						});
						return menu;
					});
				} else {
					GUIHandler.getInstance().getLogger().log(Level.SEVERE, "TopMenu required for DataSlot click handler.");
				}
			});
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
			super(Messages.GUI_ENTRY_FUNCTION_DATA_TYPE_SHOP);
			super.setData(uuid);
			super.setDataFormatter(aUuid -> aUuid == null ? Component.text("none", NamedTextColor.GRAY) : ShopHandler.getInstance().getShop(aUuid).getName());
			super.setDisplayItem(displayItem.apply(uuid));
		}

		@Override
		public Map<Action<?>, ContextConsumer<? extends TargetContext<?>>> getClickHandler() {
			return Map.of(Action.LEFT, (ContextConsumer<ClickContext>) c -> {
				if(c.getMenu() instanceof TopMenu topMenu) {
					topMenu.openSubMenu(c.getPlayer(), () -> {

						int shops = ShopHandler.getInstance().getShops().size();
						ListEditorMenu<Shop> menu = new ListEditorMenu<>(Messages.GUI_SHOPS_TITLE.asTranslatable(), Integer.max(3, Integer.min(shops / 9, 6)), new ListMenuSupplier<Shop>() {

							public Collection<Shop> getElements() {
								return ShopHandler.getInstance().getElements();
							}

							public ItemStack getDisplayItem(Shop shop) {
								return Objects.equals(shop.getUUID(), getData()) ?
										ItemStackUtils.setGlow(ShopHandler.getInstance().getDisplayItem(shop)) : ShopHandler.getInstance().getDisplayItem(shop);
							}
						});
						menu.setClickHandler(Action.LEFT, cl -> {
							setData(cl.getTarget().getUUID());
							setDisplayItem(ShopHandler.getInstance().getDisplayItem(cl.getTarget()).clone());
						});
						return menu;
					});
				} else {

					GUIHandler.getInstance().getLogger().log(Level.SEVERE, "TopMenu required for DataSlot click handler.");
				}
			});
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