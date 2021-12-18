package de.bossascrew.shops.general.util;

import com.google.common.collect.Lists;
import com.mojang.authlib.GameProfile;
import com.mojang.authlib.properties.Property;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Config;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.Limit;
import de.tr7zw.nbtapi.NBTCompound;
import de.tr7zw.nbtapi.NBTItem;
import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.TextDecoration;
import net.kyori.adventure.text.minimessage.MiniMessage;
import net.kyori.adventure.text.minimessage.Template;
import net.kyori.adventure.text.serializer.gson.GsonComponentSerializer;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemFlag;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.SkullMeta;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

@UtilityClass
public class ItemStackUtils {

	public static String HEAD_URL_ARROW_NEXT = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvMTliZjMyOTJlMTI2YTEwNWI1NGViYTcxM2FhMWIxNTJkNTQxYTFkODkzODgyOWM1NjM2NGQxNzhlZDIyYmYifX19";
	public static String HEAD_URL_ARROW_NEXT_OFF = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvOGFhMTg3ZmVkZTg4ZGUwMDJjYmQ5MzA1NzVlYjdiYTQ4ZDNiMWEwNmQ5NjFiZGM1MzU4MDA3NTBhZjc2NDkyNiJ9fX0=";
	public static String HEAD_URL_ARROW_PREV = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvYmQ2OWUwNmU1ZGFkZmQ4NGU1ZjNkMWMyMTA2M2YyNTUzYjJmYTk0NWVlMWQ0ZDcxNTJmZGM1NDI1YmMxMmE5In19fQ==";
	public static String HEAD_URL_ARROW_PREV_OFF = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvZjZkYWI3MjcxZjRmZjA0ZDU0NDAyMTkwNjdhMTA5YjVjMGMxZDFlMDFlYzYwMmMwMDIwNDc2ZjdlYjYxMjE4MCJ9fX0=";

	public static String HEAD_URL_ARROW_UP = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvMzA0MGZlODM2YTZjMmZiZDJjN2E5YzhlYzZiZTUxNzRmZGRmMWFjMjBmNTVlMzY2MTU2ZmE1ZjcxMmUxMCJ9fX0=";
	public static String HEAD_URL_ARROW_DOWN = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvNzQzNzM0NmQ4YmRhNzhkNTI1ZDE5ZjU0MGE5NWU0ZTc5ZGFlZGE3OTVjYmM1YTEzMjU2MjM2MzEyY2YifX19";

	public static String HEAD_URL_LETTER_T = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvMTU2MmU4YzFkNjZiMjFlNDU5YmU5YTI0ZTVjMDI3YTM0ZDI2OWJkY2U0ZmJlZTJmNzY3OGQyZDNlZTQ3MTgifX19";

	public static String HEAD_URL_LETTER_CHECK_MARK = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvYTkyZTMxZmZiNTljOTBhYjA4ZmM5ZGMxZmUyNjgwMjAzNWEzYTQ3YzQyZmVlNjM0MjNiY2RiNDI2MmVjYjliNiJ9fX0=";
	public static String HEAD_URL_LETTER_X = "eyJ0ZXh0dXJlcyI6eyJTS0lOIjp7InVybCI6Imh0dHA6Ly90ZXh0dXJlcy5taW5lY3JhZnQubmV0L3RleHR1cmUvYmViNTg4YjIxYTZmOThhZDFmZjRlMDg1YzU1MmRjYjA1MGVmYzljYWI0MjdmNDYwNDhmMThmYzgwMzQ3NWY3In19fQ==";

	public static Material MATERIAL_SHOP = Material.VILLAGER_SPAWN_EGG;
	public static Material MATERIAL_LIMIT = Material.HOPPER;
	public static Material MATERIAL_DISCOUNT = Material.POTION;
	public static Material MATERIAL_WEBINTERFACE = Material.ENDER_EYE;
	public static Material MATERIAL_TEMPLATE = Material.MUSIC_DISC_CHIRP;

	public static Material MATERIAL_TAGS = Material.NAME_TAG;
	public static Material MATERIAL_DATES = Material.CLOCK;
	public static Material MATERIAL_DURATIONS = Material.COMPASS;
	public static Material MATERIAL_PERMISSIONS = Material.STRUCTURE_VOID;

	public GsonComponentSerializer GSON_SERIALIZER = GsonComponentSerializer.builder().build();
	public LegacyComponentSerializer SERIALIZER = LegacyComponentSerializer.builder()
			.character('§')
			.hexColors()
			.useUnusualXRepeatedCharacterHexFormat()
			.hexCharacter('x')
			.build();
	public DurationParser DURATION_PARSER = new DurationParser(ChronoUnit.DAYS, ChronoUnit.HOURS, ChronoUnit.MINUTES);

	public void giveOrDrop(Player player, ItemStack itemStack) {
		giveOrDrop(player, itemStack, player.getLocation());
	}

	public void giveOrDrop(Player player, @Nullable ItemStack item, Location location) {

		if (item == null || item.getType() == Material.AIR) {
			return;
		}
		Map<Integer, ItemStack> leftoverItems = player.getInventory().addItem(item.clone());
		if (leftoverItems.isEmpty()) {
			return;
		}
		leftoverItems.forEach((index, item2) -> location.getWorld().dropItemNaturally(location, item2));
	}

	public ItemStack addLore(ItemStack itemStack, List<Component> lore) {
		NBTItem item = new NBTItem(itemStack);
		NBTCompound display = item.getCompound("display");
		if (display == null) {
			display = item.addCompound("display");
		}
		List<String> presentLore = display.getStringList("Lore");
		presentLore.addAll(lore.stream().map(component -> {
			return component.decoration(TextDecoration.ITALIC, component.decoration(TextDecoration.ITALIC) == TextDecoration.State.NOT_SET ?
					TextDecoration.State.FALSE : component.decoration(TextDecoration.ITALIC));
		}).map(component -> GSON_SERIALIZER.serialize(component)).collect(Collectors.toList()));
		return item.getItem();
	}

	public ItemStack setLore(ItemStack itemStack, List<Component> lore) {
		NBTItem item = new NBTItem(itemStack);
		NBTCompound display = item.getCompound("display");
		if (display == null) {
			display = item.addCompound("display");
		}
		List<String> presentLore = display.getStringList("Lore");
		presentLore.clear();
		presentLore.addAll(lore.stream().map(component -> {
			return component.decoration(TextDecoration.ITALIC, component.decoration(TextDecoration.ITALIC) == TextDecoration.State.NOT_SET ?
					TextDecoration.State.FALSE : component.decoration(TextDecoration.ITALIC));
		}).map(component -> GSON_SERIALIZER.serialize(component)).collect(Collectors.toList()));
		return item.getItem();
	}

	public void addLorePrice(List<Component> existingLore, TradeModule<?, ?> tradeModule, double discount) {
		if (tradeModule.isBuyable() && tradeModule.isSellable() && tradeModule.getPayPrice(true).equals(tradeModule.getPayPrice(false)) && discount == 1) {
			existingLore.addAll(Message.SHOP_ITEM_LORE_BOTH_PRICE.getTranslations(Template.of("price", tradeModule.getPriceDisplay(true, discount))));
		} else {
			if (tradeModule.isBuyable()) {
				existingLore.addAll(Message.SHOP_ITEM_LORE_BUY_PRICE.getTranslations(
						Template.of("price", tradeModule.getPriceDisplay(true, discount))));
			}
			if (tradeModule.isSellable()) {
				existingLore.addAll(Message.SHOP_ITEM_LORE_SELL_PRICE.getTranslations(
						Template.of("price", tradeModule.getPriceDisplay(false, discount))));
			}
		}
	}

	public void addLoreDiscount(List<Component> existingLore, List<Discount> discounts) {
		discounts.sort(Discount::compareTo);
		for (Discount discount : discounts) {
			existingLore.addAll(Message.SHOP_ITEM_LORE_DISCOUNT.getTranslations(
					Template.of("percent", discount.getFormattedPercent()),
					Template.of("name", discount.getName()),
					Template.of("start-date", TextUtils.formatLocalDateTime(discount.getNextStart())),
					Template.of("duration", DURATION_PARSER.format(discount.getDuration())),
					Template.of("remaining", DURATION_PARSER.format(discount.getRemaining()))
			));
		}
	}

	public void addLoreLimits(List<Component> existingLore, Limit userLimit, Limit globalLimit, int bought) {
		if (userLimit == null && globalLimit == null) {
			return;
		}
		Message message = userLimit == null ? Message.SHOP_ITEM_LORE_LIMIT_GLOBAL :
				globalLimit == null ? Message.SHOP_ITEM_LORE_LIMIT_PERSONAL :
						Message.SHOP_ITEM_LORE_LIMIT;
		existingLore.addAll(message.getTranslations(
				Template.of("transactioncount", bought + ""),
				Template.of("userlimit", userLimit == null ? "-" : userLimit.getTransactionLimit() + ""),
				Template.of("globallimit", globalLimit == null ? "-" : globalLimit.getTransactionLimit() + "")
		));
	}

	public void addLoreActions(List<Component> existingLore, TradeModule tradeEntry) {
		Config sc = StatShops.getInstance().getShopsConfig();
		if (tradeEntry.isBuyable()) {
			if (!sc.getBuyKeyBinding().isEmpty()) {
				getActionComponent(existingLore, sc.getBuyKeyBinding().get(0), Message.ACTION_BUY);
			}
			if (tradeEntry.isBuyableStacked()) {
				if (!sc.getBuyStackKeyBinding().isEmpty()) {
					getActionComponent(existingLore, sc.getBuyStackKeyBinding().get(0), Message.ACTION_BUY_STACK);
				}
			}
		}
		if (tradeEntry.isSellable()) {
			if (!sc.getSellKeyBinding().isEmpty()) {
				getActionComponent(existingLore, sc.getSellKeyBinding().get(0), Message.ACTION_SELL);
			}
			if (tradeEntry.isBuyableStacked()) {
				if (!sc.getSellStackKeyBinding().isEmpty()) {
					getActionComponent(existingLore, sc.getSellStackKeyBinding().get(0), Message.ACTION_SELL_STACK);
				}
			}
		}
	}

	public ItemStack createEntryItemStack(ShopEntry entry) {
		ItemStack itemStack = entry.getDisplayItem().clone();

		List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(entry, entry.getShop());;
		double discount = DiscountHandler.getInstance().combineDiscounts(discounts, false);
		List<Component> additionalLore = new ArrayList<>();

		if (entry.getModule() instanceof TradeModule tradeEntry) {

			List<String> order = StatShops.getInstance().getShopsConfig().getEntryLoreOrder();
			Component spacer = Message.SHOP_ITEM_LORE_SPACER.getTranslation();
			for (String segment : order) {
				// Place segments in order that is provided via config
				switch (segment) {
					case "price" -> {
						addLorePrice(additionalLore, tradeEntry, discount);
					}
					case "actions" -> addLoreActions(additionalLore, tradeEntry);
					case "discounts" -> {
						addLoreDiscount(additionalLore, discounts);
					}
					case "limits" -> {
						LimitsHandler.getInstance().addLimitsLore(entry, additionalLore);
					}
					case "info" -> {
						if (entry.getInfoLoreFormat() != null) {
							MiniMessage mm = StatShops.getInstance().getMiniMessage();
							additionalLore.addAll(Arrays.stream(entry.getInfoLoreFormat().split("\n")).map(mm::parse).collect(Collectors.toList()));
						}
					}
					case "spacer" -> {
						if (additionalLore.isEmpty() || !additionalLore.get(additionalLore.size() - 1).equals(spacer)) {
							additionalLore.add(spacer);
						}
					}
				}
			}
			// Remove last spacer in case it is not intended
			if (!order.isEmpty() && !order.get(order.size() - 1).equals("spacer") && !additionalLore.isEmpty() && additionalLore.get(additionalLore.size() - 1).equals(spacer)) {
				additionalLore.remove(additionalLore.size() - 1);
			}
		}
		return ItemStackUtils.addLore(itemStack, additionalLore);
	}

	private void getActionComponent(List<Component> additionalLore, String key, Message action) {
		additionalLore.addAll(Message.SHOP_ITEM_LORE_KEYBIND.getTranslations(
				Template.of("keybind", key.toLowerCase(Locale.ROOT).replace("_", "-") + "-click"),
				Template.of("action", action.getTranslation())));
	}

	public ItemStack prepareEditorEntryItemStack(ShopEntry entry) {
		return entry.getDisplayItem();
	}


	public ItemStack createItemStack(Material material, String displayName, @Nullable String lore) {
		if (lore != null) {
			List<String> loreList = Lists.newArrayList(lore.split("\n"));
			return createItemStack(material, displayName, loreList);
		}
		return createItemStack(material, displayName, (List<String>) null);
	}

	public ItemStack createItemStack(Material material, String displayName, @Nullable List<String> lore) {

		ItemStack itemStack = new ItemStack(material);
		ItemMeta meta = itemStack.getItemMeta();
		if (meta == null) {
			meta = Bukkit.getItemFactory().getItemMeta(material);
			if (meta == null) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not generate ItemMeta for ItemStack with displayname \"" + displayName + "\"");
				return itemStack;
			}
		}
		meta.setDisplayName(displayName);
		if (lore != null && !lore.isEmpty() && (lore.size() > 1 || !lore.get(0).isEmpty() || !lore.get(0).isBlank())) {
			meta.setLore(lore);
		}
		meta.addItemFlags(ItemFlag.values());
		itemStack.setItemMeta(meta);
		return itemStack;
	}

	public ItemStack createItemStack(Material material, Component displayName, List<Component> lore) {
		List<String> stringLore = lore.stream().map(component -> SERIALIZER.serialize(component)).collect(Collectors.toList());
		return createItemStack(material, SERIALIZER.serialize(displayName), stringLore);
	}

	public ItemStack createItemStack(Material material, Message name, Message lore) {
		return createItemStack(material, name.getTranslation(), lore.getTranslations());
	}

	public static ItemStack createItemStack(ItemStack itemStack, Message name, Message lore) {
		return createItemStack(itemStack, name.getTranslation(), lore.getTranslations());
	}

	public static ItemStack createItemStack(ItemStack itemStack, Component name, List<Component> lore) {
		ItemMeta meta = itemStack.getItemMeta();
		if (meta == null) {
			meta = Bukkit.getItemFactory().getItemMeta(itemStack.getType());
		}
		meta.setDisplayName(TextUtils.toLegacy(name));
		meta.setLore(lore.stream().map(TextUtils::toLegacy).collect(Collectors.toList()));
		itemStack.setItemMeta(meta);
		return itemStack;
	}

	public ItemStack createCustomHead(String url) {
		return createCustomHead(new ItemStack(Material.PLAYER_HEAD, 1), url);
	}

	public ItemStack createCustomHead(String url, Message name, Message lore) {
		return createCustomHead(createItemStack(Material.PLAYER_HEAD, name, lore), url);
	}

	public ItemStack createCustomHead(ItemStack itemStack, String url) {
		ItemMeta itemMeta = itemStack.getItemMeta();
		if (itemMeta instanceof SkullMeta meta) {
			GameProfile profile = new GameProfile(UUID.randomUUID(), null);
			profile.getProperties().put("textures", new Property("textures", url));

			try {
				Field profileField = meta.getClass().getDeclaredField("profile");
				profileField.setAccessible(true);
				profileField.set(meta, profile);

			} catch (IllegalArgumentException | NoSuchFieldException | SecurityException | IllegalAccessException error) {
				error.printStackTrace();
			}
			itemStack.setItemMeta(meta);
		} else {
			throw new UnsupportedOperationException("Trying to add a skull texture to a non-playerhead item");
		}
		return itemStack;
	}

	public ItemStack createShopItemStack(Shop shop) {
		if (shop == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(shop.getDisplayMaterial() == null ? MATERIAL_SHOP : shop.getDisplayMaterial(),
				Message.GUI_SHOPS_NAME.getTranslation(
						Template.of("name", shop.getName())),
				Message.GUI_SHOPS_LORE.getTranslations(
						Template.of("permission", shop.getPermission() == null ? "X" : shop.getPermission()),
						Template.of("name", shop.getName())));
	}

	public ItemStack createDiscountItemStack(Discount discount) {
		if (discount == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(MATERIAL_DISCOUNT,
				Message.GUI_DISCOUNTS_ENTRY_NAME.getTranslation(
						Template.of("name", discount.getName())),
				Message.GUI_DISCOUNTS_ENTRY_LORE.getTranslations(
						Template.of("percent", discount.getFormattedPercent()),
						Template.of("uuid", discount.getUuid().toString()),
						Template.of("permission", discount.getPermission() == null ? "X" : discount.getPermission()),
						Template.of("name", discount.getName()),
						Template.of("remaining", DURATION_PARSER.format(discount.getRemaining())),
						Template.of("start-date", TextUtils.formatLocalDateTime(discount.getNextStart())),
						Template.of("duration", DURATION_PARSER.format(discount.getDuration()))));
	}

	public ItemStack createLimitsItemStack(Limit limit) {
		if (limit == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(MATERIAL_LIMIT,
				Message.GUI_LIMITS_ENTRY_NAME.getTranslation(
						Template.of("name", limit.getName())),
				Message.GUI_LIMITS_ENTRY_LORE.getTranslations(
						Template.of("limit", "" + limit.getTransactionLimit()),
						Template.of("uuid", limit.getUuid().toString()),
						Template.of("global", "" + limit.isGlobal()),
						Template.of("recover", DURATION_PARSER.format(limit.getRecover()))));
	}

	public ItemStack createTemplatesItemStack(EntryTemplate template) {
		if (template == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(MATERIAL_TEMPLATE,
				Message.GUI_TEMPLATES_ENTRY_NAME.getTranslation(
						Template.of("template", template.getName())),
				Message.GUI_TEMPLATES_ENTRY_LORE.getTranslations(
						Template.of("template", template.getName()),
						Template.of("uuid", "" + template.getUuid()),
						Template.of("size", "" + template.size())));
	}

	public ItemStack setNameAndLore(ItemStack item, String displayName, String lore) {
		ItemMeta meta = item.getItemMeta();
		if (meta == null) {
			return item;
		}
		MiniMessage miniMessage = StatShops.getInstance().getMiniMessage();
		meta.setDisplayName(SERIALIZER.serialize(miniMessage.parse(displayName)));
		List<String> legacyLore = Arrays.stream(lore.split("\n")).map(s -> SERIALIZER.serialize(miniMessage.parse(s))).collect(Collectors.toList());
		meta.setLore(legacyLore);
		item.setItemMeta(meta);
		return item;
	}

	public ItemStack setGlow(ItemStack item) {
		ItemMeta meta = item.getItemMeta();
		if (meta != null) {
			meta.addEnchant(Enchantment.LUCK, 1, true);
			meta.addItemFlags(ItemFlag.HIDE_ENCHANTS);
			item.setItemMeta(meta);
		}
		return item;
	}
}
