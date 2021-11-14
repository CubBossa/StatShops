package de.bossascrew.shops.menu;

import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.util.ItemStackUtils;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public enum DefaultSpecialItem {

	//TODO translations

	EMPTY_LIGHT(Material.LIGHT_GRAY_STAINED_GLASS_PANE, " ", null),
	EMPTY_MIDDLE(Material.GRAY_STAINED_GLASS_PANE, " ", null),
	EMPTY_DARK(Material.BLACK_STAINED_GLASS_PANE, " ", null),

	PREVIOUS_PAGE(7827, Message.GENERAL_GUI_NEXT_PAGE_NAME, Message.GENERAL_GUI_NEXT_PAGE_LORE),
	NEXT_PAGE(7826, Message.GENERAL_GUI_PREV_PAGE_NAME, Message.GENERAL_GUI_PREV_PAGE_LORE),

	ACCEPT(Material.LIME_CONCRETE, Message.GENERAL_GUI_ACCEPT_NAME, Message.GENERAL_GUI_ACCEPT_LORE),
	DECLINE(Material.RED_CONCRETE, Message.GENERAL_GUI_DECLINE_NAME, Message.GENERAL_GUI_DECLINE_LORE),

	BACK(Material.SPRUCE_DOOR, Message.GENERAL_GUI_BACK_NAME, Message.GENERAL_GUI_BACK_LORE),
	ERROR(Material.BARRIER, Message.GENERAL_GUI_ERROR_NAME, Message.GENERAL_GUI_ERROR_LORE),

	//TODO lieber mit ItemStackutils.create, weil nur einmal verwendet
	MANAGER_MAIN_SHOPS(ItemStackUtils.MATERIAL_SHOP, Message.MANAGER_GUI_MAIN_SHOPS_NAME, Message.MANAGER_GUI_MAIN_SHOPS_LORE),
	MANAGER_MAIN_LIMITS(ItemStackUtils.MATERIAL_LIMIT, Message.MANAGER_GUI_MAIN_LIMITS_NAME, Message.MANAGER_GUI_MAIN_LIMITS_LORE),
	MANAGER_MAIN_DISCOUNTS(ItemStackUtils.MATERIAL_DISCOUNT, Message.MANAGER_GUI_MAIN_DISCOUNTS_NAME, Message.MANAGER_GUI_MAIN_DISCOUNTS_LORE),
	MANAGER_MAIN_WEBINTERFACE(ItemStackUtils.MATERIAL_WEBINTERFACE, Message.MANAGER_GUI_MAIN_WEBINTERFACE_NAME, Message.MANAGER_GUI_MAIN_WEBINTERFACE_LORE),

	MANAGER_SHOPS_NEW(Material.EMERALD, Message.MANAGER_GUI_SHOPS_NEW_NAME, Message.MANAGER_GUI_SHOPS_NEW_LORE),


	;

	private final ItemStack item;

	DefaultSpecialItem(Material material, String displayName, @Nullable String lore) {
		item = ItemStackUtils.createItemStack(material, displayName, lore);
	}

	DefaultSpecialItem(ItemStack item, String displayName, @Nullable String lore) {
		this.item = item.clone();
		ItemStackUtils.setNameAndLore(this.item, displayName, lore);
	}

	DefaultSpecialItem(Material material, Message displayName, Message lore) {
		this.item = ItemStackUtils.createItemStack(material, displayName.getTranslation(), lore.getTranslations());
	}

	DefaultSpecialItem(int hdbId, Message name, Message lore) {
		this.item = ItemStackUtils.createItemStack(Material.PLAYER_HEAD, name, lore);
	}

	DefaultSpecialItem(int hdbId, String displayName, @Nullable String lore) {
		this.item = ItemStackUtils.createItemStack(Material.PLAYER_HEAD, displayName, lore);
	}

	public ItemStack createSpecialItem() {
		return item.clone();
	}

}
