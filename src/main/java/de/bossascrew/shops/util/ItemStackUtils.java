package de.bossascrew.shops.util;

import com.google.common.collect.Lists;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.menu.DefaultSpecialItem;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Limit;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.entry.ShopEntry;
import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.stream.Collectors;

@UtilityClass
public class ItemStackUtils {

	public static Material MATERIAL_SHOP = Material.VILLAGER_SPAWN_EGG;
	public static Material MATERIAL_LIMIT = Material.HOPPER;
	public static Material MATERIAL_DISCOUNT = Material.POTION;
	public static Material MATERIAL_WEBINTERFACE = Material.ENDER_EYE;

	public static Material MATERIAL_TAGS = Material.NAME_TAG;
	public static Material MATERIAL_PERMISSIONS = Material.STRUCTURE_VOID;

	public LegacyComponentSerializer SERIALIZER = LegacyComponentSerializer.builder()
			.character('§')
			.hexColors()
			.useUnusualXRepeatedCharacterHexFormat()
			.hexCharacter('x')
			.build();

	public void giveOrDrop(Player player, ItemStack itemStack) {

	}

	public void addLore(ItemStack itemStack, List<Component> lore) {
		ItemMeta meta = itemStack.getItemMeta();
		if (meta == null) {
			return;
		}
		List<String> loreList = lore.stream().map(component -> SERIALIZER.serialize(component)).collect(Collectors.toList());
		if (meta.getLore() != null) {
			meta.getLore().addAll(loreList);
		} else {
			meta.setLore(loreList);
		}
		itemStack.setItemMeta(meta);
	}

	public void setLore(ItemStack itemStack, List<Component> lore) {
		ItemMeta meta = itemStack.getItemMeta();
		if (meta == null) {
			return;
		}
		meta.setLore(lore.stream().map(component -> SERIALIZER.serialize(component)).collect(Collectors.toList()));
		itemStack.setItemMeta(meta);
	}

	public List<Component> addLorePrice(ShopEntry shopEntry, List<Component> existingLore) {
		existingLore.addAll(Message.SHOP_ITEM_LORE_PRICE.getTranslations(
				Template.of("price", shopEntry.getDisplayPrice())
		));
		return existingLore;
	}

	public List<Component> addLoreDiscount(List<Component> existingLore, List<Discount> discounts) {
		for (Discount discount : discounts) {
			existingLore.addAll(Message.SHOP_ITEM_LORE_DISCOUNT.getTranslations(
					Template.of("percent", discount.getPercent() + ""),
					Template.of("name", discount.getName()),
					Template.of("start-date", discount.getStartTime() + ""),//TODO schön parsen natürlich
					Template.of("duration", discount.getDuration() + ""),
					Template.of("remaining", discount.getRemaining() + "")
			));
		}
		return existingLore;
	}

	public List<Component> addLoreLimits(List<Component> existingLore, Limit userLimit, Limit globalLimit, int bought) {
		existingLore.addAll(Message.SHOP_ITEM_LORE_LIMIT.getTranslations(
				Template.of("transactioncount", bought + ""),
				Template.of("userlimit", userLimit.getTransactionLimit() + ""),
				Template.of("globallimit", globalLimit.getTransactionLimit() + "")
		));
		return existingLore;
	}


	public ItemStack createItemStack(Material material, String displayName, @Nullable String lore) {
		if (lore != null) {
			List<String> loreList = Lists.newArrayList(lore.split("\n"));
			return createItemStack(material, displayName, loreList);
		}
		return createItemStack(material, displayName, "");
	}

	public ItemStack createItemStack(Material material, String displayName, List<String> lore) {

		ItemStack itemStack = new ItemStack(material);
		ItemMeta meta = itemStack.getItemMeta();
		if (meta == null) {
			return itemStack;
		}
		meta.setDisplayName(displayName);
		meta.setLore(lore);
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

	public ItemStack createShopItemStack(Shop shop) {
		if (shop == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(MATERIAL_SHOP,
				Message.MANAGER_GUI_SHOPS_NAME.getTranslation(
						Template.of("name", shop.getName())),
				Message.MANAGER_GUI_SHOPS_LORE.getTranslations(
						Template.of("permission", shop.getPermission() == null ? "X" : shop.getPermission()),
						Template.of("name", shop.getName()),
						Template.of("pages", "" + 1))); //TODO
	}

	public ItemStack createDiscountItemStack(Discount discount) {
		if (discount == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(MATERIAL_DISCOUNT,
				Message.MANAGER_GUI_DISCOUNTS_ENTRY_NAME.getTranslation(
						Template.of("name", discount.getName())),
				Message.MANAGER_GUI_DISCOUNTS_ENTRY_LORE.getTranslations(
						Template.of("percent", "" + discount.getPercent()),
						Template.of("uuid", discount.getUuid().toString()),
						Template.of("permission", discount.getPermission() == null ? "X" : discount.getPermission()),
						Template.of("name", discount.getName()),
						Template.of("remaining", "" + discount.getRemaining()),
						Template.of("start-date", "" + discount.getStartTime()),//TODO schön parsen natürlich
						Template.of("duration", "" + discount.getDuration())));
	}

	public ItemStack createLimitsItemStack(Limit limit) {
		if (limit == null) {
			return DefaultSpecialItem.ERROR.createSpecialItem();
		}
		return createItemStack(MATERIAL_LIMIT,
				Message.MANAGER_GUI_LIMITS_ENTRY_NAME.getTranslation(
						Template.of("limit", "" + limit.getTransactionLimit())),
				Message.MANAGER_GUI_LIMITS_ENTRY_LORE.getTranslations(
						Template.of("limit", "" + limit.getTransactionLimit()),
						Template.of("combine-transactions", "" + limit.isSummTagMemberLimits()),
						Template.of("uuid", limit.getUuid().toString()),
						Template.of("recover", "" + limit.getRecover()))); //TODO nice format
	}

	public ItemStack createPlayerHead(int id) {
		return null;
	}

	public ItemStack createPlayerHead(OfflinePlayer player) {
		return null;
	}

	public void setNameAndLore(ItemStack item, String displayName, String lore) {

	}
}
