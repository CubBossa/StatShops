package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.menuframework.inventory.MenuPresets;
import de.cubbossa.translations.Message;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.function.Supplier;

public class Icon {

	public static final ItemStack STACK_EMPTY_LIGHT = icon(Material.LIGHT_GRAY_STAINED_GLASS_PANE, " ", null);
	public static final ItemStack STACK_EMPTY_LIGHT_RP = icon(Material.LIGHT_GRAY_STAINED_GLASS_PANE, " ", null, 7122000);
	public static final ItemStack STACK_EMPTY_MIDDLE = icon(Material.GRAY_STAINED_GLASS_PANE, " ", null);
	public static final ItemStack STACK_EMPTY_DARK_SIMPLE = icon(Material.BLACK_STAINED_GLASS_PANE, " ", null);
	public static final ItemStack STACK_EMPTY_DARK = icon(Material.BLACK_STAINED_GLASS_PANE, " ", null, 7122000);
	public static final ItemStack STACK_EMPTY_DARK_RP = icon(Material.BLACK_STAINED_GLASS_PANE, " ", null, 7122001);
	public static final ItemStack STACK_PREV_PAGE = icon(ItemStackUtils.HEAD_URL_ARROW_PREV, Messages.GENERAL_GUI_PREV_PAGE_NAME, Messages.GENERAL_GUI_PREV_PAGE_LORE);
	public static final ItemStack STACK_PREV_PAGE_RP = icon(ItemStackUtils.HEAD_URL_ARROW_PREV, Messages.GENERAL_GUI_PREV_PAGE_NAME, Messages.GENERAL_GUI_PREV_PAGE_LORE, 7122000);
	public static final ItemStack STACK_PREV_PAGE_OFF = icon(ItemStackUtils.HEAD_URL_ARROW_PREV_OFF, Messages.GENERAL_GUI_PREV_PAGE_NAME, Messages.GENERAL_GUI_PREV_PAGE_LORE);
	public static final ItemStack STACK_PREV_PAGE_OFF_RP = icon(ItemStackUtils.HEAD_URL_ARROW_PREV_OFF, Messages.GENERAL_GUI_PREV_PAGE_NAME, Messages.GENERAL_GUI_PREV_PAGE_LORE, 7122000);
	public static final ItemStack STACK_NEXT_PAGE = icon(ItemStackUtils.HEAD_URL_ARROW_NEXT, Messages.GENERAL_GUI_NEXT_PAGE_NAME, Messages.GENERAL_GUI_NEXT_PAGE_LORE);
	public static final ItemStack STACK_NEXT_PAGE_RP = icon(ItemStackUtils.HEAD_URL_ARROW_NEXT, Messages.GENERAL_GUI_NEXT_PAGE_NAME, Messages.GENERAL_GUI_NEXT_PAGE_LORE, 7122000);
	public static final ItemStack STACK_NEXT_PAGE_OFF = icon(ItemStackUtils.HEAD_URL_ARROW_NEXT_OFF, Messages.GENERAL_GUI_NEXT_PAGE_NAME, Messages.GENERAL_GUI_NEXT_PAGE_LORE);
	public static final ItemStack STACK_NEXT_PAGE_OFF_RP = icon(ItemStackUtils.HEAD_URL_ARROW_NEXT_OFF, Messages.GENERAL_GUI_NEXT_PAGE_NAME, Messages.GENERAL_GUI_NEXT_PAGE_LORE, 7122000);
	public static final ItemStack STACK_ACCEPT_RP = icon(ItemStackUtils.HEAD_URL_LETTER_CHECK_MARK, Messages.GENERAL_GUI_ACCEPT_NAME, Messages.GENERAL_GUI_ACCEPT_LORE, 7122000);
	public static final ItemStack STACK_DECLINE_RP = icon(ItemStackUtils.HEAD_URL_LETTER_X, Messages.GENERAL_GUI_DECLINE_NAME, Messages.GENERAL_GUI_DECLINE_LORE, 7122000);
	public static final ItemStack STACK_WARNING_RP = icon(ItemStackUtils.HEAD_URL_LETTER_EXCLAMATION, Messages.GENERAL_GUI_WARNING_NAME, Messages.GENERAL_GUI_WARNING_LORE, 7122000);
	public static final ItemStack STACK_BACK = icon(Material.SPRUCE_DOOR, Messages.GENERAL_GUI_BACK_NAME, Messages.GENERAL_GUI_BACK_LORE);
	public static final ItemStack STACK_ERROR = icon(Material.BARRIER, Messages.GENERAL_GUI_ERROR_NAME, Messages.GENERAL_GUI_ERROR_LORE);

	public static final Supplier<ItemStack> EMPTY_LIGHT = () -> STACK_EMPTY_LIGHT;
	public static final Supplier<ItemStack> EMPTY_LIGHT_RP = () -> STACK_EMPTY_LIGHT_RP;
	public static final Supplier<ItemStack> EMPTY_MIDDLE = () -> STACK_EMPTY_MIDDLE;
	public static final Supplier<ItemStack> EMPTY_DARK_SIMPLE = () -> STACK_EMPTY_DARK_SIMPLE;
	public static final Supplier<ItemStack> EMPTY_DARK = () -> STACK_EMPTY_DARK;
	public static final Supplier<ItemStack> EMPTY_DARK_RP = () -> STACK_EMPTY_DARK_RP;
	public static final Supplier<ItemStack> PREV_PAGE = () -> STACK_PREV_PAGE;
	public static final Supplier<ItemStack> PREV_PAGE_RP = () -> STACK_PREV_PAGE_RP;
	public static final Supplier<ItemStack> PREV_PAGE_OFF = () -> STACK_PREV_PAGE_OFF;
	public static final Supplier<ItemStack> PREV_PAGE_OFF_RP = () -> STACK_PREV_PAGE_OFF_RP;
	public static final Supplier<ItemStack> NEXT_PAGE = () -> STACK_NEXT_PAGE;
	public static final Supplier<ItemStack> NEXT_PAGE_RP = () -> STACK_NEXT_PAGE_RP;
	public static final Supplier<ItemStack> NEXT_PAGE_OFF = () -> STACK_NEXT_PAGE_OFF;
	public static final Supplier<ItemStack> NEXT_PAGE_OFF_RP = () -> STACK_NEXT_PAGE_OFF_RP;
	public static final Supplier<ItemStack> ACCEPT_RP = () -> STACK_ACCEPT_RP;
	public static final Supplier<ItemStack> DECLINE_RP = () -> STACK_DECLINE_RP;
	public static final Supplier<ItemStack> WARNING_RP = () -> STACK_WARNING_RP;
	public static final Supplier<ItemStack> BACK = () -> STACK_BACK;
	public static final Supplier<ItemStack> ERROR = () -> STACK_ERROR;

	static {
		MenuPresets.BACK = Icon.STACK_BACK.clone();
		MenuPresets.LEFT = Icon.STACK_PREV_PAGE_RP.clone();
		MenuPresets.LEFT_DISABLED = Icon.STACK_PREV_PAGE_OFF_RP.clone();
		MenuPresets.RIGHT = Icon.STACK_NEXT_PAGE_RP.clone();
		MenuPresets.RIGHT_DISABLED = Icon.STACK_NEXT_PAGE_OFF_RP.clone();
	}

	private static ItemStack icon(Material material, String displayName, @Nullable String lore) {
		return ItemStackUtils.createItemStack(material, displayName, lore);
	}

	private static ItemStack icon(Material material, String displayName, @Nullable String lore, int customModelData) {
		ItemStack item = ItemStackUtils.createItemStack(material, displayName, lore);
		return ItemStackUtils.setCustomModelData(item, customModelData);
	}

	private static ItemStack icon(Material material, Message displayName, Message lore) {
		return ItemStackUtils.createItemStack(material, displayName, lore.asComponents());
	}

	private static ItemStack icon(Material material, Message displayName, Message lore, int customModelData) {
		ItemStack item = ItemStackUtils.createItemStack(material, displayName, lore.asComponents());
		return ItemStackUtils.setCustomModelData(item, customModelData);
	}

	private static ItemStack icon(String headUrl, Message name, Message lore) {
		return ItemStackUtils.createCustomHead(headUrl, name, lore);
	}

	private static ItemStack icon(String headUrl, Message name, Message lore, int customModelData) {
		ItemStack item = ItemStackUtils.createCustomHead(headUrl, name, lore);
		return ItemStackUtils.setCustomModelData(item, customModelData);
	}
}
