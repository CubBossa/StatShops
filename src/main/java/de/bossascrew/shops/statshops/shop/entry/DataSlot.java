package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.menu.ListMenu;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.menu.contexts.TargetContext;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.configuration.serialization.ConfigurationSerializable;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
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
	private ContextConsumer<TargetContext<ClickType, Runnable>> clickHandler = c -> {
	};
	private @Nullable T data;
	private Consumer<T> updateHandler = t -> {
	};
	private Function<T, Component> dataFormatter = t -> Component.text("" + t);

	public DataSlot(Message typeMessage) {
		this.typeMessage = typeMessage;
	}

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
		return ItemStackUtils.setNameAndLore(displayItem, typeMessage.getTranslation(TagResolver.resolver("name", Tag.inserting(name.getTranslation()))),
				lore.getTranslations(TagResolver.resolver("current", Tag.inserting(dataFormatter.apply(data)))));
	}

	public static class TextSlot extends DataSlot<String> {

		public TextSlot(String data) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_STRING);
			super.setClickHandler(clickContext -> {
				Player player = clickContext.getPlayer();
				player.closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("" + getData())
						.title(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_STRING.getLegacyTranslation(TagResolver.resolver("name", Tag.inserting(getName().getTranslation()))))
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> clickContext.getTarget().run(), 1L))
						.onComplete((p, s) -> {
							setData(s);
							clickContext.getTarget().run();
							return AnvilGUI.Response.close();
						}).open(player);
			});
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
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
			super.setClickHandler(clickContext -> {
				Player player = clickContext.getPlayer();
				player.closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("" + getData())
						.title(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION.getLegacyTranslation(TagResolver.resolver("name", Tag.inserting(getName().getTranslation()))))
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> clickContext.getTarget().run(), 1L))
						.onComplete((p, s) -> {
							setData(s);
							clickContext.getTarget().run();
							return AnvilGUI.Response.close();
						}).open(player);
			});
			super.setData(data);
			super.setDisplayItem(new ItemStack(Material.COMMAND_BLOCK));
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
			super.setClickHandler(clickContext -> {
				setData(Boolean.FALSE.equals(getData()));
				super.setDisplayItem(ItemStackUtils.createButtonItemStack(Boolean.TRUE.equals(getData()), Message.NONE, Message.NONE));
			});
			super.setDisplayItem(ItemStackUtils.createButtonItemStack(data, Message.NONE, Message.NONE));
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
			super.setClickHandler(clickContext -> {
				Player player = clickContext.getPlayer();
				player.closeInventory();
				new AnvilGUI.Builder()
						.plugin(StatShops.getInstance())
						.text("" + getData())
						.title(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER.getLegacyTranslation(TagResolver.resolver("name", Tag.inserting(getName().getTranslation()))))
						.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> clickContext.getTarget().run(), 1L))
						.onComplete((p, s) -> {
							try {
								setData(Double.parseDouble(s.replace(" ", "")));
								super.setDisplayItem(new ItemStack(Material.PAPER, Integer.min(Integer.max(1, getData().intValue()), 64)));
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
			return shop == null ? new ItemStack(Material.VILLAGER_SPAWN_EGG) : shop.getListDisplayItem();
		};

		public ShopSlot(UUID uuid) {
			super(Message.GUI_ENTRY_FUNCTION_DATA_TYPE_SHOP);

			super.setData(uuid);
			super.setClickHandler(clickContext -> {
				int shops = ShopHandler.getInstance().getShops().size();
				ListMenu<Shop> menu = new ListMenu<>(Integer.max(3, Integer.min(shops % 9, 6)), ShopHandler.getInstance(),
						Message.GUI_SHOPS_TITLE, backContext -> clickContext.getTarget().run());
				menu.setGlowPredicate(s -> Objects.equals(s.getUUID(), getData()));
				menu.setClickHandler(cc -> {
					this.setData(cc.getTarget().getUUID());
					setDisplayItem(cc.getTarget().getListDisplayItem().clone());
					clickContext.getTarget().run();
				});
				clickContext.getTarget().run();
				menu.openInventory(clickContext.getPlayer());
			});
			super.setDataFormatter(aUuid -> aUuid == null ? Component.text("none", NamedTextColor.GRAY) : ShopHandler.getInstance().getShop(aUuid).getName());
			super.setDisplayItem(displayItem.apply(uuid));
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