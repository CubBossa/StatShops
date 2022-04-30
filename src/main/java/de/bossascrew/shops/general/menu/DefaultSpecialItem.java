package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.data.Message;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public enum DefaultSpecialItem {

	EMPTY_LIGHT(Material.LIGHT_GRAY_STAINED_GLASS_PANE, " ", null),
	EMPTY_LIGHT_RP(Material.LIGHT_GRAY_STAINED_GLASS_PANE, " ", null, 7122000),
	EMPTY_MIDDLE(Material.GRAY_STAINED_GLASS_PANE, " ", null),
	EMPTY_DARK_SIMPLE(Material.BLACK_STAINED_GLASS_PANE, " ", null),
	EMPTY_DARK(Material.BLACK_STAINED_GLASS_PANE, " ", null, 7122000),
	EMPTY_DARK_RP(Material.BLACK_STAINED_GLASS_PANE, " ", null, 7122001),

	PREV_PAGE(ItemStackUtils.HEAD_URL_ARROW_PREV, Message.GENERAL_GUI_PREV_PAGE_NAME, Message.GENERAL_GUI_PREV_PAGE_LORE),
	PREV_PAGE_RP(ItemStackUtils.HEAD_URL_ARROW_PREV, Message.GENERAL_GUI_PREV_PAGE_NAME, Message.GENERAL_GUI_PREV_PAGE_LORE, 7122000),
	PREV_PAGE_OFF(ItemStackUtils.HEAD_URL_ARROW_PREV_OFF, Message.GENERAL_GUI_PREV_PAGE_NAME, Message.GENERAL_GUI_PREV_PAGE_LORE),
	PREV_PAGE_OFF_RP(ItemStackUtils.HEAD_URL_ARROW_PREV_OFF, Message.GENERAL_GUI_PREV_PAGE_NAME, Message.GENERAL_GUI_PREV_PAGE_LORE, 7122000),
	NEXT_PAGE(ItemStackUtils.HEAD_URL_ARROW_NEXT, Message.GENERAL_GUI_NEXT_PAGE_NAME, Message.GENERAL_GUI_NEXT_PAGE_LORE),
	NEXT_PAGE_RP(ItemStackUtils.HEAD_URL_ARROW_NEXT, Message.GENERAL_GUI_NEXT_PAGE_NAME, Message.GENERAL_GUI_NEXT_PAGE_LORE, 7122000),
	NEXT_PAGE_OFF(ItemStackUtils.HEAD_URL_ARROW_NEXT_OFF, Message.GENERAL_GUI_NEXT_PAGE_NAME, Message.GENERAL_GUI_NEXT_PAGE_LORE),
	NEXT_PAGE_OFF_RP(ItemStackUtils.HEAD_URL_ARROW_NEXT_OFF, Message.GENERAL_GUI_NEXT_PAGE_NAME, Message.GENERAL_GUI_NEXT_PAGE_LORE, 7122000),

	ACCEPT_RP(ItemStackUtils.HEAD_URL_LETTER_CHECK_MARK, Message.GENERAL_GUI_ACCEPT_NAME, Message.GENERAL_GUI_ACCEPT_LORE, 7122000),
	DECLINE_RP(ItemStackUtils.HEAD_URL_LETTER_X, Message.GENERAL_GUI_DECLINE_NAME, Message.GENERAL_GUI_DECLINE_LORE, 7122000),

	BACK(Material.SPRUCE_DOOR, Message.GENERAL_GUI_BACK_NAME, Message.GENERAL_GUI_BACK_LORE),
	ERROR(Material.BARRIER, Message.GENERAL_GUI_ERROR_NAME, Message.GENERAL_GUI_ERROR_LORE),
	;

	private final ItemStack item;

	DefaultSpecialItem(Material material, String displayName, @Nullable String lore) {
		item = ItemStackUtils.createItemStack(material, displayName, lore);
	}

	DefaultSpecialItem(Material material, String displayName, @Nullable String lore, int customModelData) {
		item = ItemStackUtils.createItemStack(material, displayName, lore);
		ItemStackUtils.setCustomModelData(item, customModelData);
	}

	DefaultSpecialItem(ItemStack item, String displayName, @Nullable String lore) {
		this.item = item.clone();
		ItemStackUtils.setNameAndLore(this.item, displayName, lore);
	}

	DefaultSpecialItem(Material material, Message displayName, Message lore) {
		this.item = ItemStackUtils.createItemStack(material, displayName.getTranslation(), lore.getTranslations());
	}

	DefaultSpecialItem(Material material, Message displayName, Message lore, int customModelData) {
		this.item = ItemStackUtils.createItemStack(material, displayName.getTranslation(), lore.getTranslations());
		ItemStackUtils.setCustomModelData(item, customModelData);
	}

	DefaultSpecialItem(String headUrl, Message name, Message lore) {
		this.item = ItemStackUtils.createCustomHead(headUrl, name, lore);
	}

	DefaultSpecialItem(String headUrl, Message name, Message lore, int customModelData) {
		this.item = ItemStackUtils.createCustomHead(headUrl, name, lore);
		ItemStackUtils.setCustomModelData(item, customModelData);
	}

	public ItemStack create() {
		return item.clone();
	}

}
