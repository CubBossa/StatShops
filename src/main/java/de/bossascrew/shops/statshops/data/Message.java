package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.handler.TranslationHandler;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.MiniMessage;
import net.kyori.adventure.text.minimessage.Template;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

public class Message {

	public static final Message NONE = new Message("");
	public static final Message PREFIX = new Message("general.prefix");

	public static final Message GENERAL_NO_PERMISSION = new Message("general.no_permission");
	public static final Message GENERAL_PLUGIN_LOADING = new Message("general.plugin_loading");
	public static final Message GENERAL_CONFIG_RELOADED_IN_MS = new Message("general.config_reloaded");
	public static final Message GENERAL_CONFIG_RELOAD_ERROR = new Message("general.config_error");
	public static final Message GENERAL_LANGUAGE_RELOADED_IN_MS = new Message("general.language_reloaded");
	public static final Message GENERAL_LANGUAGE_RELOAD_ERROR = new Message("general.language_error");
	public static final Message GENERAL_GUI_BACK_NAME = new Message("general.gui.back.name");
	public static final Message GENERAL_GUI_BACK_LORE = new Message("general.gui.back.lore");
	public static final Message GENERAL_GUI_ERROR_NAME = new Message("general.gui.error.name");
	public static final Message GENERAL_GUI_ERROR_LORE = new Message("general.gui.error.lore");
	public static final Message GENERAL_GUI_NEXT_PAGE_NAME = new Message("general.gui.next_page.name");
	public static final Message GENERAL_GUI_NEXT_PAGE_LORE = new Message("general.gui.next_page.lore");
	public static final Message GENERAL_GUI_PREV_PAGE_NAME = new Message("general.gui.prev_page.name");
	public static final Message GENERAL_GUI_PREV_PAGE_LORE = new Message("general.gui.prev_page.lore");
	public static final Message GENERAL_GUI_ACCEPT_NAME = new Message("general.gui.accept.name");
	public static final Message GENERAL_GUI_ACCEPT_LORE = new Message("general.gui.accept.lore");
	public static final Message GENERAL_GUI_DECLINE_NAME = new Message("general.gui.decline.name");
	public static final Message GENERAL_GUI_DECLINE_LORE = new Message("general.gui.decline.lore");
	public static final Message GENERAL_GUI_LIST_INFO_NAME = new Message("general.gui.list_info.name");
	public static final Message GENERAL_GUI_LIST_INFO_LORE = new Message("general.gui.list_info.lore");
	public static final Message GENERAL_WEBINTERFACE_LOADING = new Message("general.webinterface.loading");
	public static final Message GENERAL_WEBINTERFACE_LINK = new Message("general.webinterface.link");
	public static final Message GENERAL_WEBINTERFACE_ERROR = new Message("general.webinterface.error");
	public static final Message GENERAL_EDITABLE_CURRENTLY_EDITED = new Message("general.editable_edited");

	public static final Message CITIZENS_ASSIGNED = new Message("citizens.assign.success");
	public static final Message CITIZENS_CONFIRM_OVERRIDE = new Message("citizens.assign.confirm");
	public static final Message CITIZENS_CLICK_TO_ASSIGN = new Message("citizens.assign.info");
	public static final Message ACTION_BUY = new Message("shop.actions.buy");
	public static final Message ACTION_SELL = new Message("shop.actions.sell");
	public static final Message ACTION_BUY_STACK = new Message("shop.actions.buy_stack");
	public static final Message ACTION_SELL_STACK = new Message("shop.actions.sell_stack");
	public static final Message SHOP_NO_PERMISSION = new Message("shop.no_permission");
	public static final Message SHOP_NOT_ENABLED = new Message("shop.not_enabled");
	public static final Message SHOP_COOLDOWN = new Message("shop.cooldown");
	public static final Message SHOP_GUI_TITLE = new Message("shop.gui.title",
			"Defines the title of the shop menu",
			new Pair<>("name", "<gradient:dark_green:green:dark_green>Example Shop"),
			new Pair<>("page", "" + 2),
			new Pair<>("mode", "<gold>Sell</gold>"),
			new Pair<>("pages", "" + 3));
	public static final Message SHOP_ITEM_LORE_SPACER = new Message("shop.gui.item.lore.spacer");
	public static final Message SHOP_ITEM_LORE_BOTH_PRICE = new Message("shop.gui.item.lore.price");
	public static final Message SHOP_ITEM_LORE_BUY_PRICE = new Message("shop.gui.item.lore.buy_price");
	public static final Message SHOP_ITEM_LORE_SELL_PRICE = new Message("shop.gui.item.lore.sell_price");
	public static final Message SHOP_ITEM_LORE_KEYBIND = new Message("shop.gui.item.lore.keybinding");
	public static final Message SHOP_ITEM_LORE_DISCOUNT = new Message("shop.gui.item.lore.discount");
	public static final Message SHOP_ITEM_LORE_DISCOUNT_POSITIVE = new Message("shop.gui.item.lore.discount_positive");
	public static final Message SHOP_ITEM_LORE_DISCOUNT_NEGATIVE = new Message("shop.gui.item.lore.discount_negative");
	public static final Message SHOP_ITEM_LORE_LIMIT = new Message("shop.gui.item.lore.limit_both");
	public static final Message SHOP_ITEM_LORE_LIMIT_PERSONAL = new Message("shop.gui.item.lore.limit_personal");
	public static final Message SHOP_ITEM_LORE_LIMIT_GLOBAL = new Message("shop.gui.item.lore.limit_global");
	public static final Message SHOP_MODE_BUY_NAME = new Message("shop.modes.buy.name");
	public static final Message SHOP_MODE_BUY_LORE = new Message("shop.modes.buy.lore");
	public static final Message SHOP_MODE_SELL_NAME = new Message("shop.modes.sell.name");
	public static final Message SHOP_MODE_SELL_LORE = new Message("shop.modes.sell.lore");
	public static final Message SHOP_MODE_TRADE_NAME = new Message("shop.modes.trade.name");
	public static final Message SHOP_MODE_TRADE_LORE = new Message("shop.modes.trade.lore");
	public static final Message SHOP_TRADE_FEEDBACK_PROMPT_FORMAT = new Message("shop.prompt_feedback");
	public static final Message SHOP_TRADE_FEEDBACK_CUMUL_FORMAT = new Message("shop.cumulative_feedback");
	public static final Message SHOP_TRADE_FEEDBACK_CUMUL_TITLE = new Message("shop.cumulative_title");
	public static final Message SHOP_TRADE_FEEDBACK_PAY = new Message("shop.trade_feedback_indicator_pay");
	public static final Message SHOP_TRADE_FEEDBACK_GAIN = new Message("shop.trade_feedback_indicator_gain");

	public static final Message VILLAGER_SHOP_TITLE = new Message("villager_shop.gui.title");

	public static final Message GUI_MAIN_TITLE = new Message("manager.gui.main.title");
	public static final Message GUI_MAIN_SHOPS_NAME = new Message("manager.gui.main.shops.name");
	public static final Message GUI_MAIN_SHOPS_LORE = new Message("manager.gui.main.shops.lore");
	public static final Message GUI_MAIN_DISCOUNTS_NAME = new Message("manager.gui.main.discounts.name");
	public static final Message GUI_MAIN_DISCOUNTS_LORE = new Message("manager.gui.main.discounts.lore");
	public static final Message GUI_MAIN_LIMITS_NAME = new Message("manager.gui.main.limits.name");
	public static final Message GUI_MAIN_LIMITS_LORE = new Message("manager.gui.main.limits.lore");
	public static final Message GUI_MAIN_LANGUAGE_NAME = new Message("manager.gui.main.language.name");
	public static final Message GUI_MAIN_LANGUAGE_LORE = new Message("manager.gui.main.language.lore");
	public static final Message GUI_MAIN_WEBINTERFACE_NAME = new Message("manager.gui.main.webinterface.name");
	public static final Message GUI_MAIN_WEBINTERFACE_LORE = new Message("manager.gui.main.webinterface.lore");
	public static final Message GUI_SHOPS_TITLE = new Message("manager.gui.shops.title");
	public static final Message GUI_SHOPS_DELETE_CONFIRM = new Message("manager.gui.shops.confirm_delete");
	public static final Message GUI_SHOPS_NEW_TITLE = new Message("manager.gui.shops.new_shop.title");
	public static final Message GUI_SHOPS_NEW_NAME = new Message("manager.gui.shops.new_shop.name");
	public static final Message GUI_SHOPS_NEW_LORE = new Message("manager.gui.shops.new_shop.lore");
	public static final Message GUI_SHOPS_NAME = new Message("manager.gui.shops.entry.name");
	public static final Message GUI_SHOPS_LORE = new Message("manager.gui.shops.entry.lore");
	public static final Message GUI_SHOPS_ALREADY_EDITED = new Message("manager.gui.shops.already_edited");

	public static final Message GUI_SHOP_SET_NAME_TITLE = new Message("manager.gui.shop.set_name.title");
	public static final Message GUI_SHOP_SET_NAME_NAME = new Message("manager.gui.shop.set_name.name");
	public static final Message GUI_SHOP_SET_NAME_LORE = new Message("manager.gui.shop.set_name.lore");
	public static final Message GUI_SHOP_SET_PERMISSION_TITLE = new Message("manager.gui.shop.set_permission.title");
	public static final Message GUI_SHOP_SET_PERMISSION_NAME = new Message("manager.gui.shop.set_permission.name");
	public static final Message GUI_SHOP_SET_PERMISSION_LORE = new Message("manager.gui.shop.set_permission.lore");
	public static final Message GUI_SHOP_SET_TAGS_NAME = new Message("manager.gui.shop.set_tags.name");
	public static final Message GUI_SHOP_SET_TAGS_LORE = new Message("manager.gui.shop.set_tags.lore");
	public static final Message GUI_SHOP_SET_LIMITS_NAME = new Message("manager.gui.shop.set_limits.name");
	public static final Message GUI_SHOP_SET_LIMITS_LORE = new Message("manager.gui.shop.set_limits.lore");
	public static final Message GUI_SHOP_SET_DISCOUNTS_NAME = new Message("manager.gui.shop.set_discounts.name");
	public static final Message GUI_SHOP_SET_DISCOUNTS_LORE = new Message("manager.gui.shop.set_discounts.lore");
	public static final Message GUI_SHOP_SET_TEMPLATE_NAME = new Message("manager.gui.shop.set_template.name");
	public static final Message GUI_SHOP_SET_TEMPLATE_LORE = new Message("manager.gui.shop.set_template.lore");
	public static final Message GUI_SHOP_SET_NPC_NAME = new Message("manager.gui.shop.set_citizens.name",
			"The name of the item in the shop menu, that allows you to assign this shop to an citizens npc.");
	public static final Message GUI_SHOP_SET_NPC_LORE = new Message("manager.gui.shop.set_citizens.lore",
			"The lore of the item in the shop menu, that allows you to assign this shop to an citizens npc.");
	public static final Message GUI_SHOP_SET_ENABLED_NAME = new Message("manager.gui.shop.set_enabled.name");
	public static final Message GUI_SHOP_SET_ENABLED_LORE = new Message("manager.gui.shop.set_enabled.lore");
	public static final Message GUI_SHOP_SET_REMEMBER_PAGE_NAME = new Message("manager.gui.shop.set_remember_page.name");
	public static final Message GUI_SHOP_SET_REMEMBER_PAGE_LORE = new Message("manager.gui.shop.set_remember_page.lore");
	public static final Message GUI_SHOP_SET_REMEMBER_MODE_NAME = new Message("manager.gui.shop.set_remember_mode.name");
	public static final Message GUI_SHOP_SET_REMEMBER_MODE_LORE = new Message("manager.gui.shop.set_remember_mode.lore");
	public static final Message GUI_SHOP_SET_CONTENT_NAME = new Message("manager.gui.shop.set_content.name");
	public static final Message GUI_SHOP_SET_CONTENT_LORE = new Message("manager.gui.shop.set_content.lore");
	public static final Message GUI_SHOP_SET_PREVIEW_NAME = new Message("manager.gui.shop.preview.name");
	public static final Message GUI_SHOP_SET_PREVIEW_LORE = new Message("manager.gui.shop.preview.lore");
	public static final Message GUI_SHOP_SET_DEFAULT_MODE_NAME = new Message("manager.gui.shop.set_default_mode.name");
	public static final Message GUI_SHOP_SET_DEFAULT_MODE_LORE = new Message("manager.gui.shop.set_default_mode.lore");
	public static final Message GUI_SHOP_SET_DEFAULT_PAGE_NAME = new Message("manager.gui.shop.set_default_page.name");
	public static final Message GUI_SHOP_SET_DEFAULT_PAGE_LORE = new Message("manager.gui.shop.set_default_page.lore");
	public static final Message GUI_SHOP_SET_ROWS_NAME = new Message("manager.gui.shop.set_rows.name");
	public static final Message GUI_SHOP_SET_ROWS_LORE = new Message("manager.gui.shop.set_rows.lore");
	public static final Message GUI_SHOP_LIMITS_TITLE = new Message("manager.gui.shop.limits.title");
	public static final Message GUI_SHOP_LIMITS_INFO_NAME = new Message("manager.gui.shop.limits.info.name");
	public static final Message GUI_SHOP_LIMITS_INFO_LORE = new Message("manager.gui.shop.limits.info.lore");
	public static final Message GUI_SHOP_DISCOUNTS_TITLE = new Message("manager.gui.shop.discounts.title");
	public static final Message GUI_SHOP_DISCOUNTS_INFO_NAME = new Message("manager.gui.shop.discounts.info.name");
	public static final Message GUI_SHOP_DISCOUNTS_INFO_LORE = new Message("manager.gui.shop.discounts.info.lore");
	public static final Message GUI_SHOP_TEMPLATE_TITLE = new Message("manager.gui.shop.template.title");
	public static final Message GUI_SHOP_TEMPLATE_INFO_NAME = new Message("manager.gui.shop.template.info.name");
	public static final Message GUI_SHOP_TEMPLATE_INFO_LORE = new Message("manager.gui.shop.template.info.lore");
	public static final Message GUI_TAGS_TITLE = new Message("manager.gui.tags.title");
	public static final Message GUI_TAGS_NEW_TAG_TITLE = new Message("manager.gui.tags.new_tag.title");
	public static final Message GUI_TAGS_NEW_TAG_NAME = new Message("manager.gui.tags.new_tag.name");
	public static final Message GUI_TAGS_NEW_TAG_LORE = new Message("manager.gui.tags.new_tag.lore");

	public static final Message GENERAL_GUI_TAGS_INFO_NAME = new Message("manager.gui.tags.info.name");
	public static final Message GENERAL_GUI_TAGS_INFO_LORE = new Message("manager.gui.tags.info.lore");
	public static final Message GENERAL_GUI_TAGS_REMOVE_TAG = new Message("manager.gui.tags.remove_tag");

	public static final Message GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME = new Message("manager.gui.shop_editor.template.name");
	public static final Message GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE = new Message("manager.gui.shop_editor.template.lore");
	public static final Message GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME = new Message("manager.gui.shop_editor.freeze.name");
	public static final Message GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE = new Message("manager.gui.shop_editor.freeze.lore");

	public static final Message GUI_LIMITS = new Message("manager.gui.limits.title");
	public static final Message GUI_LIMITS_ALREADY_EDITED = new Message("manager.gui.limits.already_edited");
	public static final Message GUI_LIMITS_DELETE_CONFIRM = new Message("manager.gui.limits.confirm_delete");
	public static final Message GUI_LIMITS_ENTRY_NAME = new Message("manager.gui.limits.entry.name");
	public static final Message GUI_LIMITS_ENTRY_LORE = new Message("manager.gui.limits.entry.lore");
	public static final Message GUI_LIMITS_NEW_TITLE = new Message("manager.gui.limits.new.title");
	public static final Message GUI_LIMITS_NEW_NAME = new Message("manager.gui.limits.new.name");
	public static final Message GUI_LIMITS_NEW_LORE = new Message("manager.gui.limits.new.lore");
	public static final Message GUI_LIMIT_SET_NAME_TITLE = new Message("manager.gui.limit.set_name.title");
	public static final Message GUI_LIMIT_SET_NAME_NAME = new Message("manager.gui.limit.set_name.name");
	public static final Message GUI_LIMIT_SET_NAME_LORE = new Message("manager.gui.limit.set_name.lore");
	public static final Message GUI_LIMIT_SET_TAGS_NAME = new Message("manager.gui.limit.set_tags.name");
	public static final Message GUI_LIMIT_SET_TAGS_LORE = new Message("manager.gui.limit.set_tags.lore");
	public static final Message GUI_LIMIT_SET_PERMISSION_TITLE = new Message("manager.gui.limit.set_permission.title");
	public static final Message GUI_LIMIT_SET_PERMISSION_NAME = new Message("manager.gui.limit.set_permission.name");
	public static final Message GUI_LIMIT_SET_PERMISSION_LORE = new Message("manager.gui.limit.set_permission.lore");

	public static final Message GUI_DISCOUNTS = new Message("manager.gui.discounts.title");
	public static final Message GUI_DISCOUNTS_DELETE_CONFIRM = new Message("manager.gui.discounts.confirm_delete");
	public static final Message GUI_DISCOUNTS_ALREADY_EDITED = new Message("manager.gui.discounts.already_edited");
	public static final Message GUI_DISCOUNTS_ENTRY_NAME = new Message("manager.gui.discounts.entry.name");
	public static final Message GUI_DISCOUNTS_ENTRY_LORE = new Message("manager.gui.discounts.entry.lore");
	public static final Message GUI_DISCOUNTS_NEW_TITLE = new Message("manager.gui.discounts.new.title");
	public static final Message GUI_DISCOUNTS_NEW_NAME = new Message("manager.gui.discounts.new.name");
	public static final Message GUI_DISCOUNTS_NEW_LORE = new Message("manager.gui.discounts.new.lore");

	public static final Message GUI_DISCOUNT = new Message("manager.gui.discount.title");
	public static final Message GUI_DISCOUNT_SET_NAME_TITLE = new Message("manager.gui.discount.set_name.title");
	public static final Message GUI_DISCOUNT_SET_NAME_NAME = new Message("manager.gui.discount.set_name.name");
	public static final Message GUI_DISCOUNT_SET_NAME_LORE = new Message("manager.gui.discount.set_name.lore");
	public static final Message GUI_DISCOUNT_SET_PERMISSION_TITLE = new Message("manager.gui.discount.set_permission.title");
	public static final Message GUI_DISCOUNT_SET_PERMISSION_NAME = new Message("manager.gui.discount.set_permission.name");
	public static final Message GUI_DISCOUNT_SET_PERMISSION_LORE = new Message("manager.gui.discount.set_permission.lore");
	public static final Message GUI_DISCOUNT_SET_TAGS_NAME = new Message("manager.gui.discount.set_tags.name");
	public static final Message GUI_DISCOUNT_SET_TAGS_LORE = new Message("manager.gui.discount.set_tags.lore");
	public static final Message GUI_DISCOUNT_SET_DURATION_TITLE = new Message("manager.gui.discount.set_duration.title");
	public static final Message GUI_DISCOUNT_SET_DURATION_NAME = new Message("manager.gui.discount.set_duration.name");
	public static final Message GUI_DISCOUNT_SET_DURATION_LORE = new Message("manager.gui.discount.set_duration.lore");
	public static final Message GUI_DISCOUNT_SET_START_TITLE = new Message("manager.gui.discount.set_start.title");
	public static final Message GUI_DISCOUNT_SET_START_NAME = new Message("manager.gui.discount.set_start.name");
	public static final Message GUI_DISCOUNT_SET_START_LORE = new Message("manager.gui.discount.set_start.lore");
	public static final Message GUI_DISCOUNT_SET_PERCENT_TITLE = new Message("manager.gui.discount.set_percent.title");
	public static final Message GUI_DISCOUNT_SET_PERCENT_NAME = new Message("manager.gui.discount.set_percent.name");
	public static final Message GUI_DISCOUNT_SET_PERCENT_LORE = new Message("manager.gui.discount.set_percent.lore");
	public static final Message GUI_DISCOUNT_START_INFO_NAME = new Message("manager.gui.discount.start.info.name");
	public static final Message GUI_DISCOUNT_START_INFO_LORE = new Message("manager.gui.discount.start.info.lore");
	public static final Message GUI_DISCOUNT_START_NEW_TITLE = new Message("manager.gui.discount.start.new.title");
	public static final Message GUI_DISCOUNT_START_NEW_NAME = new Message("manager.gui.discount.start.new.name");
	public static final Message GUI_DISCOUNT_START_NEW_LORE = new Message("manager.gui.discount.start.new.lore");
	public static final Message GUI_DISCOUNT_START_DELETE_CONFIRM = new Message("manager.gui.discount.start.confirm_delete");

	public static final Message GUI_SHOP_ENTRY = new Message("manager.gui.shop_entry.title");
	public static final Message GUI_TEMPLATES = new Message("manager.gui.templates.title");
	public static final Message GUI_TEMPLATES_CHOOSE = new Message("manager.gui.templates.choose_title");
	public static final Message GUI_TEMPLATES_APPLY = new Message("manager.gui.templates.apply_title");
	public static final Message GUI_TEMPLATES_NEW = new Message("manager.gui.templates.new_title");
	public static final Message GUI_TEMPLATES_ENTRY_NAME = new Message("manager.gui.templates.entry.name",
			"Sets the display name of a template entry in the templates menu",
			new Pair<>("template", "<white>Default Shop Layout"));
	public static final Message GUI_TEMPLATES_ENTRY_LORE = new Message("manager.gui.templates.entry.lore",
			"Sets the lore of a template entry in the templates menu",
			new Pair<>("template", "<white>Default Shop Layout"),
			new Pair<>("uuid", UUID.randomUUID().toString()),
			new Pair<>("size", "7"));

	public static final Message GUI_ENTRY_TITLE = new Message("manager.gui.entry.title");
	public static final Message GUI_ENTRY_SET_LORE_NAME = new Message("manager.gui.entry.set_lore.name");
	public static final Message GUI_ENTRY_SET_LORE_LORE = new Message("manager.gui.entry.set_lore.lore");
	public static final Message GUI_ENTRY_SET_PERMISSION_TITLE = new Message("manager.gui.entry.set_permission.title");
	public static final Message GUI_ENTRY_SET_PERMISSION_NAME = new Message("manager.gui.entry.set_permission.name");
	public static final Message GUI_ENTRY_SET_PERMISSION_LORE = new Message("manager.gui.entry.set_permission.lore");
	public static final Message GUI_ENTRY_SET_TAGS_NAME = new Message("manager.gui.entry.set_tags.name");
	public static final Message GUI_ENTRY_SET_TAGS_LORE = new Message("manager.gui.entry.set_tags.lore");
	public static final Message GUI_ENTRY_SET_FUNCTION_TITLE = new Message("manager.gui.entry.set_function.title");
	public static final Message GUI_ENTRY_SET_FUNCTION_NAME = new Message("manager.gui.entry.set_function.name",
			new Pair<>("name", "<white>Static"));
	public static final Message GUI_ENTRY_SET_FUNCTION_LORE = new Message("manager.gui.entry.set_function.lore",
			new Pair<>("function", "<white>Static"));
	public static final Message GUI_ENTRY_SET_COSTS_TITLE = new Message("manager.gui.entry.set_costs.title");

	public static final Message GUI_ENTRY_FUNCTION_STATIC_NAME = new Message("manager.gui.entry.defaults.static.name");
	public static final Message GUI_ENTRY_FUNCTION_STATIC_LORE = new Message("manager.gui.entry.defaults.static.lore");
	public static final Message GUI_ENTRY_FUNCTION_CLOSE_NAME = new Message("manager.gui.entry.defaults.close.name");
	public static final Message GUI_ENTRY_FUNCTION_CLOSE_LORE = new Message("manager.gui.entry.defaults.close.lore");
	public static final Message GUI_ENTRY_FUNCTION_PREV_PAGE_NAME = new Message("manager.gui.entry.defaults.prev_page.name");
	public static final Message GUI_ENTRY_FUNCTION_PREV_PAGE_LORE = new Message("manager.gui.entry.defaults.prev_page.lore");
	public static final Message GUI_ENTRY_FUNCTION_NEXT_PAGE_NAME = new Message("manager.gui.entry.defaults.next_page.name");
	public static final Message GUI_ENTRY_FUNCTION_NEXT_PAGE_LORE = new Message("manager.gui.entry.defaults.next_page.lore");
	public static final Message GUI_ENTRY_FUNCTION_EXACT_PAGE_NAME = new Message("manager.gui.entry.defaults.exact_page.name");
	public static final Message GUI_ENTRY_FUNCTION_EXACT_PAGE_LORE = new Message("manager.gui.entry.defaults.exact_page.lore");
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_ITEM_NAME = new Message("manager.gui.entry.defaults.article_item.name");
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_ITEM_LORE = new Message("manager.gui.entry.defaults.article_item.lore");
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_CMD_NAME = new Message("manager.gui.entry.defaults.article_cmd.name");
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_CMD_LORE = new Message("manager.gui.entry.defaults.article_cmd.lore");
	public static final Message GUI_ENTRY_FUNCTION_COSTS_ITEM_NAME = new Message("manager.gui.entry.defaults.costs_item.name");
	public static final Message GUI_ENTRY_FUNCTION_COSTS_ITEM_LORE = new Message("manager.gui.entry.defaults.costs_item.lore");
	public static final Message GUI_ENTRY_FUNCTION_COSTS_XP_NAME = new Message("manager.gui.entry.defaults.costs_xp.name");
	public static final Message GUI_ENTRY_FUNCTION_COSTS_XP_LORE = new Message("manager.gui.entry.defaults.costs_xp.lore");
	public static final Message GUI_ENTRY_FUNCTION_COSTS_VAULT_NAME = new Message("manager.gui.entry.defaults.costs_vault.name");
	public static final Message GUI_ENTRY_FUNCTION_COSTS_VAULT_LORE = new Message("manager.gui.entry.defaults.costs_vault.lore");

	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL = new Message("manager.gui.entry.function.type.bool");
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER = new Message("manager.gui.entry.function.type.int");
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION = new Message("manager.gui.entry.function.type.equation");
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_STRING = new Message("manager.gui.entry.function.type.string");
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK = new Message("manager.gui.entry.function.type.itemstack");

	public static final Message GUI_ENTRY_FUNCTION_PAGE_NAME = new Message("manager.gui.entry.function.data.page.name");
	public static final Message GUI_ENTRY_FUNCTION_PAGE_LORE = new Message("manager.gui.entry.function.data.page.lore");
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_NAME = new Message("manager.gui.entry.function.data.purchasable.name");
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_LORE = new Message("manager.gui.entry.function.data.purchasable.lore");
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_NAME = new Message("manager.gui.entry.function.data.purchasable_stacked.name");
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_LORE = new Message("manager.gui.entry.function.data.purchasable_stacked.lore");
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_NAME = new Message("manager.gui.entry.function.data.sellable.name");
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_LORE = new Message("manager.gui.entry.function.data.sellable.lore");
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_STACKED_NAME = new Message("manager.gui.entry.function.data.sellable_stacked.name");
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_STACKED_LORE = new Message("manager.gui.entry.function.data.sellable_stacked.lore");
	public static final Message GUI_ENTRY_FUNCTION_GAIN_AMOUNT_NAME = new Message("manager.gui.entry.function.data.gain_amount.name");
	public static final Message GUI_ENTRY_FUNCTION_GAIN_AMOUNT_LORE = new Message("manager.gui.entry.function.data.gain_amount.lore");
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_NAME = new Message("manager.gui.entry.function.data.buy_price_amount.name");
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_LORE = new Message("manager.gui.entry.function.data.buy_price_amount.lore");
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_NAME = new Message("manager.gui.entry.function.data.sell_price_amount.name");
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_LORE = new Message("manager.gui.entry.function.data.sell_price_amount.lore");
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_NAME = new Message("manager.gui.entry.function.data.buy_price_equation.name");
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_LORE = new Message("manager.gui.entry.function.data.buy_price_equation.lore");
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_NAME = new Message("manager.gui.entry.function.data.sell_price_equation.name");
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_LORE = new Message("manager.gui.entry.function.data.sell_price_equation.lore");
	public static final Message GUI_ENTRY_FUNCTION_GAIN_ITEM_NAME = new Message("manager.gui.entry.function.data.gain_item.name");
	public static final Message GUI_ENTRY_FUNCTION_GAIN_ITEM_LORE = new Message("manager.gui.entry.function.data.gain_item.lore");
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_NAME = new Message("manager.gui.entry.function.data.buy_price_item.name");
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_LORE = new Message("manager.gui.entry.function.data.buy_price_item.lore");
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_NAME = new Message("manager.gui.entry.function.data.sell_price_item.name");
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_LORE = new Message("manager.gui.entry.function.data.sell_price_item.lore");


	@Getter
	private final String key;
	@Getter
	private final String comment;
	@Getter
	private final Pair<String, String>[] examplePlaceholders;

	@SafeVarargs
	public Message(String key, Pair<String, String>... examplePlaceholders) {
		this(key, "", examplePlaceholders);
	}

	@SafeVarargs
	public Message(String key, String comment, Pair<String, String>... examplePlaceholders) {
		this.key = key;
		this.comment = comment;
		this.examplePlaceholders = examplePlaceholders;
	}

	public Component getTranslation(Template... templates) {
		List<Template> t = new ArrayList<>(List.of(templates));
		if (!this.equals(Message.PREFIX)) {
			t.add(Template.of("prefix", Message.PREFIX.getTranslation()));
		}
		String format = TranslationHandler.getInstance().getMessage(key);
		return StatShops.getInstance().getMiniMessage().parse(format, t);
	}

	public List<Component> getTranslations(Template...templates) {
		String[] toFormat = TranslationHandler.getInstance().getMessage(key).split("\n");
		List<Component> result = new ArrayList<>();
		MiniMessage miniMessage = StatShops.getInstance().getMiniMessage();
		for (String string : toFormat) {
			result.add(miniMessage.parse(string, templates));
		}
		if (result.stream().allMatch(component -> component.equals(Component.text("")))) {
			return new ArrayList<>();
		}
		return result;
	}

	public String getLegacyTranslation(Template... templates) {
		return TextUtils.toLegacy(getTranslation(templates));
	}

	public List<String> getLegacyTranslations(Template... templates) {
		return getTranslations(templates).stream().map(TextUtils::toLegacy).collect(Collectors.toList());
	}

	public static List<Message> values() {
		List<Message> messages = new ArrayList<>();
		Field[] fields = Message.class.getDeclaredFields();
		for (Field field : fields) {
			try {
				if (field.getType().equals(Message.class) && Modifier.isStatic(field.getModifiers())) {
					messages.add((Message) field.get(null));
				}
			} catch (IllegalAccessException ignored) {
			}
		}
		return messages;
	}
}
