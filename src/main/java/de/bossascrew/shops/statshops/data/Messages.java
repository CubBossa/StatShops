package de.bossascrew.shops.statshops.data;

import de.cubbossa.translations.Message;
import de.cubbossa.translations.MessageGroupMeta;
import de.cubbossa.translations.MessageMeta;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

public class Messages {

	public static final Message NONE = new Message("");

	@MessageMeta("<gray>")
	public static final Message COL_BASE = new Message("color.base");
	@MessageMeta("<white>")
	public static final Message COL_HIGHLIGHT = new Message("color.highlight");
	@MessageMeta("<yellow>")
	public static final Message COL_WARNING = new Message("color.warn");
	@MessageMeta("<red>")
	public static final Message COL_SEVERE = new Message("color.severe");

	@MessageMeta("<bold><gold>StatShops</gold> </bold><gray>» ")
	public static final Message PREFIX = new Message("general.prefix");
	@MessageMeta("<prefix><red>You don't have permission to do this.")
	public static final Message GENERAL_NO_PERMISSION = new Message("general.no_permission");
	@MessageMeta("<prefix><red>The plugin is currently loading. Try again in a few seconds.")
	public static final Message GENERAL_PLUGIN_LOADING = new Message("general.plugin_loading");
	@MessageMeta(value = "<prefix><gray>Config file reloaded in <green><ms><dark_green>ms<gray>.",
			placeholders = "ms")
	public static final Message GENERAL_CONFIG_RELOADED_IN_MS = new Message("general.config_reloaded");
	@MessageMeta("<prefix><red>Could not reload the config file.")
	public static final Message GENERAL_CONFIG_RELOAD_ERROR = new Message("general.config_error");
	@MessageMeta(value = "<prefix><gray>Language file reloaded in <green><ms><dark_green>ms<gray>.",
			placeholders = "ms")
	public static final Message GENERAL_LANGUAGE_RELOADED_IN_MS = new Message("general.language_reloaded");
	@MessageMeta("<prefix><red>Could not reload the language file.")
	public static final Message GENERAL_LANGUAGE_RELOAD_ERROR = new Message("general.language_error");
	@MessageMeta("<gold>Back")
	public static final Message GENERAL_GUI_BACK_NAME = new Message("general.gui.back.name");
	public static final Message GENERAL_GUI_BACK_LORE = new Message("general.gui.back.lore");
	@MessageMeta("<red>Error")
	public static final Message GENERAL_GUI_ERROR_NAME = new Message("general.gui.error.name");
	public static final Message GENERAL_GUI_ERROR_LORE = new Message("general.gui.error.lore");
	@MessageMeta("<gold>Next page")
	public static final Message GENERAL_GUI_NEXT_PAGE_NAME = new Message("general.gui.next_page.name");
	public static final Message GENERAL_GUI_NEXT_PAGE_LORE = new Message("general.gui.next_page.lore");
	@MessageMeta("<gold>Previous page")
	public static final Message GENERAL_GUI_PREV_PAGE_NAME = new Message("general.gui.prev_page.name");
	public static final Message GENERAL_GUI_PREV_PAGE_LORE = new Message("general.gui.prev_page.lore");
	@MessageMeta("<green>Accept")
	public static final Message GENERAL_GUI_ACCEPT_NAME = new Message("general.gui.accept.name");
	public static final Message GENERAL_GUI_ACCEPT_LORE = new Message("general.gui.accept.lore");
	@MessageMeta("<red>Decline")
	public static final Message GENERAL_GUI_DECLINE_NAME = new Message("general.gui.decline.name");
	public static final Message GENERAL_GUI_DECLINE_LORE = new Message("general.gui.decline.lore");
	@MessageMeta("<#F97C1B>Invalid")
	public static final Message GENERAL_GUI_WARNING_NAME = new Message("general.gui.warning.name");
	public static final Message GENERAL_GUI_WARNING_LORE = new Message("general.gui.warning.lore");
	@MessageMeta("<gold>Info")
	public static final Message GENERAL_GUI_LIST_INFO_NAME = new Message("general.gui.list_info.name");
	@MessageMeta("""
			<gray>» <yellow>left-click: <gray>edit element
			<gray>» <yellow>middle-click: <gray>duplicate element
			<gray>» <yellow>right-click: <gray>delete element""")
	public static final Message GENERAL_GUI_LIST_INFO_LORE = new Message("general.gui.list_info.lore");
	@MessageMeta("<prefix>Generating session...")
	public static final Message GENERAL_WEBINTERFACE_LOADING = new Message("general.webinterface.loading");
	@MessageMeta(value = "<prefix><green><click:open_url:<link>><hover:show_text:Open webinterface>Link to websession: <link></hover></click>", placeholders = "link")
	public static final Message GENERAL_WEBINTERFACE_LINK = new Message("general.webinterface.link");
	@MessageMeta("<prefix><red>An error occured while generating webinterface session.")
	public static final Message GENERAL_WEBINTERFACE_ERROR = new Message("general.webinterface.error");
	@MessageMeta(value = "<prefix><red>This object is currently edited by <editor>", placeholders = "editor")
	public static final Message GENERAL_EDITABLE_CURRENTLY_EDITED = new Message("general.editable_edited");
	@MessageMeta(value = "<prefix><gray><amount> items were found and added to <shop>.", placeholders = {"amount", "shop"})
	public static final Message GENERAL_CHEST_PARSED = new Message("general.chest_content_parsed");
	@MessageMeta("<prefix><red>You have to look at a chest to perform this command.")
	public static final Message GENERAL_NO_CHEST = new Message("general.no_chest_found");

	@MessageGroupMeta(path = "error.parse", placeholders = "format")
	@MessageMeta("<gray>A date with the following\n<gray>format is required:\n<aqua><format>")
	public static final Message ERROR_PARSE_STRING = new Message("error.parse.string");
	@MessageMeta("<gray>A string of the following\n<gray>type is required:\n<aqua><format>")
	public static final Message ERROR_PARSE_INTEGER = new Message("error.parse.integer");
	@MessageMeta("<gray>An integer number is required.")
	public static final Message ERROR_PARSE_DOUBLE = new Message("error.parse.double");
	@MessageMeta("<gray>A float number is required.")
	public static final Message ERROR_PARSE_PERCENT = new Message("error.parse.percent");
	@MessageMeta("<gray>A percentage is required:\n<aqua><format>")
	public static final Message ERROR_PARSE_DATE = new Message("error.parse.date");
	@MessageMeta("<gray>A duration with the following\n<gray>format is required:\n<aqua><format>")
	public static final Message ERROR_PARSE_DURATION = new Message("error.parse.duration");

	@MessageMeta(value = "<prefix><gray>Data Preset successfully saved as <white><name>", placeholders = "name")
	public static final Message DATA_PRESET_EXPORT_SUCCESS = new Message("general.data_presets.export_success");
	@MessageMeta(value = "<prefix><gray>Export <shops>, <discounts>, <limits> and <templates> as <white><name></white>? [<green><click:run_command:/statshops accept>accept</click></green>] [<red><click:run_command:/statshops decline>decline</click></red>]",
			placeholders = {"shops", "discounts", "limits", "templates", "name"})
	public static final Message DATA_PRESET_EXPORT_CONFIRM = new Message("general.data_presets.export_confirmation");
	@MessageMeta("<prefix><gray>Data Preset successfully imported.")
	public static final Message DATA_PRESET_IMPORT_SUCCESS = new Message("general.data_presets.import_success");
	@MessageMeta(value = "<prefix><gray>Import <shops>, <discounts>, <limits> and <templates> from <white><name></white>? [<green><click:run_command:/statshops accept>accept</click></green>] [<red><click:run_command:/statshops decline>decline</click></red>]",
			placeholders = {"shops", "discounts", "limits", "templates", "name"})
	public static final Message DATA_PRESET_IMPORT_CONFIRM = new Message("general.data_presets.import_confirmation");
	@MessageMeta("<prefix><red><name> is not a valid file.")
	public static final Message DATA_PRESET_NOT_A_FILE = new Message("general.data_presets.import_not_a_file");

	@MessageMeta("<prefix><gray>Shop was successfully assigned to NPC. Use <click:suggest_command:/trait remove statshop><hover:show_text:\"Click to execute\"><white><italic>/trait remove statshop</italic></white></hover></click> to remove trait.")
	public static final Message CITIZENS_ASSIGNED = new Message("citizens.assign.success");
	@MessageMeta("<prefix><red>This NPC already has a shop trait. Click again to override.")
	public static final Message CITIZENS_CONFIRM_OVERRIDE = new Message("citizens.assign.confirm");
	@MessageMeta("<prefix><gray>Click a Citizens NPC to bind this shop to it. Use <yellow>left-click</yellow> to cancel.")
	public static final Message CITIZENS_CLICK_TO_ASSIGN = new Message("citizens.assign.info");
	@MessageMeta("<prefix><gray>Action cancelled.")
	public static final Message CITIZENS_CANCELLED = new Message("citizens.assign.cancelled");

	@MessageMeta("Buy")
	public static final Message ACTION_BUY = new Message("shop.actions.buy");
	@MessageMeta("Sell")
	public static final Message ACTION_SELL = new Message("shop.actions.sell");
	@MessageMeta("Buy Stack")
	public static final Message ACTION_BUY_STACK = new Message("shop.actions.buy_stack");
	@MessageMeta("Sell Stack")
	public static final Message ACTION_SELL_STACK = new Message("shop.actions.sell_stack");
	@MessageMeta("<red>You don't have permission for this shop.")
	public static final Message SHOP_NO_PERMISSION = new Message("shop.no_permission");
	@MessageMeta("<red>This shop is currently disabled.")
	public static final Message SHOP_NOT_ENABLED = new Message("shop.not_enabled");
	@MessageMeta("<red>Please slow down.")
	public static final Message SHOP_COOLDOWN = new Message("shop.cooldown");
	@MessageMeta(value = "<name> <page-title> - Page <page>/<pages>",
			placeholders = {"name", "page-title", "page", "pages"})
	public static final Message SHOP_GUI_TITLE = new Message("shop.gui.title");
	@MessageMeta("<gradient:#24054F:#2F0768:#24054F>---------------------------</gradient>")
	public static final Message SHOP_ITEM_LORE_SPACER = new Message("shop.gui.item.lore.spacer");
	@MessageMeta(value = "<gray>Buy/Sell Price: <yellow><price>", placeholders = "price")
	public static final Message SHOP_ITEM_LORE_BOTH_PRICE = new Message("shop.gui.item.lore.price");
	@MessageMeta(value = "<gray>Buy Price: <yellow><price>", placeholders = "price")
	public static final Message SHOP_ITEM_LORE_BUY_PRICE = new Message("shop.gui.item.lore.buy_price");
	@MessageMeta(value = "<gray>Sell Price: <yellow><price>", placeholders = "price")
	public static final Message SHOP_ITEM_LORE_SELL_PRICE = new Message("shop.gui.item.lore.sell_price");
	@MessageMeta(value = "<gray>» <yellow><keybind>: <gray><action>", placeholders = {"keybind", "action"})
	public static final Message SHOP_ITEM_LORE_KEYBIND = new Message("shop.gui.item.lore.keybinding");
	@MessageMeta(value = "<white><percent>% <name>",
			placeholders = {"name", "percent", "start-date", "duration", "remaining"})
	public static final Message SHOP_ITEM_LORE_DISCOUNT = new Message("shop.gui.item.lore.discount");
	@MessageMeta(
			value = "<red>+<discount>",
			comment = "The way to display positive / negative discounts. (Negative discounts -> user needs less money to buy item)",
			placeholders = "discount"
	)
	public static final Message SHOP_ITEM_LORE_DISCOUNT_POSITIVE = new Message("shop.gui.item.lore.discount_positive");
	@MessageMeta(value = "<green><discount>", placeholders = "discount")
	public static final Message SHOP_ITEM_LORE_DISCOUNT_NEGATIVE = new Message("shop.gui.item.lore.discount_negative");
	@MessageMeta(value = """
			<gray><red><transactioncount></red>/<dark_red><userlimit></dark_red> personal limit</gray>
			<gray><red><transactioncount></red>/<dark_red><globallimit></dark_red> global limit
			<gray>Recover: <yellow><recovery_duration>""", placeholders = {"transactioncount", "userlimit", "globallimit", "recovery_duration"})
	public static final Message SHOP_ITEM_LORE_LIMIT = new Message("shop.gui.item.lore.limit_both");
	@MessageMeta("<gray><red><transactioncount></red>/<dark_red><userlimit></dark_red> personal limit</gray>\n<gray>Recover: <yellow><recovery_duration>")
	public static final Message SHOP_ITEM_LORE_LIMIT_PERSONAL = new Message("shop.gui.item.lore.limit_personal");
	@MessageMeta("<gray><red><transactioncount></red>/<dark_red><globallimit></dark_red> global limit\n<gray>Recover: <yellow><recovery_duration>")
	public static final Message SHOP_ITEM_LORE_LIMIT_GLOBAL = new Message("shop.gui.item.lore.limit_global");
	@MessageMeta(value = "<indicator><transaction>", comment = """
			Valid placeholders:
			   <indicator>:       "<green>+" if object received, "<red>-" if object paid (general.trade_feedback_indicator_gain/pay)
			   <transaction>:     The transaction, already formatted by the currency. Check the currency format for more control.""")
	public static final Message SHOP_TRADE_FEEDBACK_PROMPT_FORMAT = new Message("shop.prompt_feedback");
	@MessageMeta(value = "<indicator><transaction>", comment = """
			Valid placeholders:
			   <indicator>:       "<green>+" if object received, "<red>-" if object paid (general.trade_feedback_indicator_gain/pay)
			   <transaction>:     The transaction, already formatted by the currency""")
	public static final Message SHOP_TRADE_FEEDBACK_CUMUL_FORMAT = new Message("shop.cumulative_feedback");
	@MessageMeta("<white><underlined>Shopping balance")
	public static final Message SHOP_TRADE_FEEDBACK_CUMUL_TITLE = new Message("shop.cumulative_title");
	@MessageMeta("<red>-</red>")
	public static final Message SHOP_TRADE_FEEDBACK_PAY = new Message("shop.trade_feedback_indicator_pay");
	@MessageMeta("<green>+</green>")
	public static final Message SHOP_TRADE_FEEDBACK_GAIN = new Message("shop.trade_feedback_indicator_gain");

	@MessageMeta(value = "<name>", placeholders = "name")
	public static final Message VILLAGER_SHOP_TITLE = new Message("villager_shop.gui.title");

	@MessageMeta("Setup your shop system")
	public static final Message GUI_MAIN_TITLE = new Message("manager.gui.main.title");
	@MessageMeta("<dark_green>Shops")
	public static final Message GUI_MAIN_SHOPS_NAME = new Message("manager.gui.main.shops.name");
	@MessageMeta("<gray>Click here to create \n<gray>and manage your shops.")
	public static final Message GUI_MAIN_SHOPS_LORE = new Message("manager.gui.main.shops.lore");
	@MessageMeta("<dark_purple>Discounts")
	public static final Message GUI_MAIN_DISCOUNTS_NAME = new Message("manager.gui.main.discounts.name");
	@MessageMeta("<gray>Click here to create \n<gray>and manage trade discounts.")
	public static final Message GUI_MAIN_DISCOUNTS_LORE = new Message("manager.gui.main.discounts.lore");
	@MessageMeta("<dark_gray>Limits")
	public static final Message GUI_MAIN_LIMITS_NAME = new Message("manager.gui.main.limits.name");
	@MessageMeta("<gray>Click here to create \n<gray>and manage trade limits.")
	public static final Message GUI_MAIN_LIMITS_LORE = new Message("manager.gui.main.limits.lore");
	@MessageMeta("<gold>Language")
	public static final Message GUI_MAIN_LANGUAGE_NAME = new Message("manager.gui.main.language.name");
	@MessageMeta(value = "<gray>» <yellow>left-click: <gray>open web editor\n<gray>» <yellow>right-click: <gray>reload language from <file>",
			placeholders = "file")
	public static final Message GUI_MAIN_LANGUAGE_LORE = new Message("manager.gui.main.language.lore");
	@MessageMeta("<dark_purple>Webinterface")
	public static final Message GUI_MAIN_WEBINTERFACE_NAME = new Message("manager.gui.main.webinterface.name");
	@MessageMeta("<gray>Click to generate a \n<gray>Link for the webinterface.")
	public static final Message GUI_MAIN_WEBINTERFACE_LORE = new Message("manager.gui.main.webinterface.lore");
	@MessageMeta("Click a shop to edit")
	public static final Message GUI_SHOPS_TITLE = new Message("manager.gui.shops.title");
	@MessageMeta(value = "<red>Delete Shop <name>?", placeholders = "name")
	public static final Message GUI_SHOPS_DELETE_CONFIRM = new Message("manager.gui.shops.confirm_delete");
	@MessageMeta(value = "Create a new Shop", comment = "The title of the anvil GUI")
	public static final Message GUI_SHOPS_NEW_TITLE = new Message("manager.gui.shops.new_shop.title");
	@MessageMeta("<green>New Shop")
	public static final Message GUI_SHOPS_NEW_NAME = new Message("manager.gui.shops.new_shop.name");
	@MessageMeta("<gray>Click to create a\n<gray>new shop.")
	public static final Message GUI_SHOPS_NEW_LORE = new Message("manager.gui.shops.new_shop.lore");
	@MessageMeta("Choose a shop type")
	public static final Message GUI_SHOPS_TYPE_TITLE = new Message("manager.gui.shops.new_shop_type.title");
	@MessageGroupMeta(path = "manager.gui.shops.entry",
			comment = "Defines, how each shop will be displayed in the list",
			placeholders = "name")
	@MessageMeta("<green><name>")
	public static final Message GUI_SHOPS_NAME = new Message("manager.gui.shops.entry.name");
	@MessageMeta(value = "<gray>Perm: <white><permission>", placeholders = "permission")
	public static final Message GUI_SHOPS_LORE = new Message("manager.gui.shops.entry.lore");

	@MessageMeta("<white>Chest Menu Shop")
	public static final Message SHOP_TYPE_CHEST_NAME = new Message("manager.shop_types.chest_menu_shop.name");
	@MessageMeta("""
			<gray>This shop type is based on a
			<gray>chest inventory and can be
			<gray>modified in row size.
			<gray>» <yellow>Limits & Discounts
			<gray>» <yellow>Pagination
			<gray>» <yellow>Templates""")
	public static final Message SHOP_TYPE_CHEST_LORE = new Message("manager.shop_types.chest_menu_shop.lore");
	@MessageMeta("<white>Villager Shop")
	public static final Message SHOP_TYPE_VILLAGER_NAME = new Message("manager.shop_types.villager_shop.name");
	@MessageMeta("""
			<gray>This shop type simulates the
			<gray>trade inventory of a merchant.
			<gray>» <yellow>Limits & Discounts""")
	public static final Message SHOP_TYPE_VILLAGER_LORE = new Message("manager.shop_types.villager_shop.lore");

	@MessageMeta(value = "Change Shop Name", comment = "The title of the anvil GUI")
	public static final Message GUI_SHOP_SET_NAME_TITLE = new Message("manager.gui.shop.set_name.title");
	@MessageMeta("<white>Set Name and Material")
	public static final Message GUI_SHOP_SET_NAME_NAME = new Message("manager.gui.shop.set_name.name");
	@MessageMeta("""
			<gray>Click to change the
			<gray>Name of the shop.

			<gray>To <rainbow>format</rainbow> your name
			<gray>use the Kyori MiniMessage format

			<gray>Click this icon with an item
			<gray>in your Hand to change the display
			<gray>material in guis.""")
	public static final Message GUI_SHOP_SET_NAME_LORE = new Message("manager.gui.shop.set_name.lore");
	@MessageMeta(value = "Change Shop Permission", comment = "The title of the anvil GUI")
	public static final Message GUI_SHOP_SET_PERMISSION_TITLE = new Message("manager.gui.shop.set_permission.title");
	@MessageMeta("<white>Set Shop Permission")
	public static final Message GUI_SHOP_SET_PERMISSION_NAME = new Message("manager.gui.shop.set_permission.name");
	@MessageMeta(value = """
			<gray>Current: <red><permission>

			<gray>Only players with this
			<gray>permission will be able to
			<gray>open this shop.

			<gray>Type "null" to reset.""",
			placeholders = "permission")
	public static final Message GUI_SHOP_SET_PERMISSION_LORE = new Message("manager.gui.shop.set_permission.lore");
	@MessageMeta("<white>Open Tag Menu")
	public static final Message GUI_SHOP_SET_TAGS_NAME = new Message("manager.gui.shop.set_tags.name");
	@MessageMeta("""
			<gray>Discounts and Limits can be
			<gray>applied to all Shops and Shopentries
			<gray>that have certain tags.

			<gray>Use the tags menu to configure
			<gray>tags for this shop.""")
	public static final Message GUI_SHOP_SET_TAGS_LORE = new Message("manager.gui.shop.set_tags.lore");
	@MessageMeta("<white>Apply Limits")
	public static final Message GUI_SHOP_SET_LIMITS_NAME = new Message("manager.gui.shop.set_limits.name");
	@MessageMeta("""
			<gray>Here you can apply
			<gray>a limit to the whole shop at once.
			<gray>This can be also archieved
			<gray>by using tags. With this
			<gray>menu wanted to provide a more
			<gray>userfriendly interface.""")
	public static final Message GUI_SHOP_SET_LIMITS_LORE = new Message("manager.gui.shop.set_limits.lore");
	@MessageMeta("<white>Apply Discounts")
	public static final Message GUI_SHOP_SET_DISCOUNTS_NAME = new Message("manager.gui.shop.set_discounts.name");
	@MessageMeta("""
			<gray>Here you can apply
			<gray>a discount to the whole shop at once.
			<gray>This can be also archieved
			<gray>by using tags. With this
			<gray>menu wanted to provide a more
			<gray>userfriendly interface.""")
	public static final Message GUI_SHOP_SET_DISCOUNTS_LORE = new Message("manager.gui.shop.set_discounts.lore");
	@MessageMeta("<white>Apply Default Template")
	public static final Message GUI_SHOP_SET_TEMPLATE_NAME = new Message("manager.gui.shop.set_template.name");
	@MessageMeta(value = """
			<gray>Current: <red><template>

			<gray>Here you can set
			<gray>a default template.
			<gray>It will automatically be applied
			<gray>to each new shop page.

			<gray>It does not change any
			<gray> existing pages.""", placeholders = "template")
	public static final Message GUI_SHOP_SET_TEMPLATE_LORE = new Message("manager.gui.shop.set_template.lore");
	@MessageMeta("<white>Citizens NPC Binding")
	public static final Message GUI_SHOP_SET_NPC_NAME = new Message("manager.gui.shop.set_citizens.name");
	@MessageMeta(value = """
			<gray>Click to bind this shop
			<gray>to a Citizens NPC.

			<gray>Unbind with
			<white><italic>/npc select <#ID></italic></white> <gray>and
			<white><italic>/trait remove statshop

			<gray>This Shop is bound to:
			<gray>» <gold><current>""", placeholders = "current")
	public static final Message GUI_SHOP_SET_NPC_LORE = new Message("manager.gui.shop.set_citizens.lore");
	@MessageMeta(value = "<white>Remember Page: <value>", placeholders = "value")
	public static final Message GUI_SHOP_SET_REMEMBER_PAGE_NAME = new Message("manager.gui.shop.set_remember_page.name");
	@MessageMeta("""
			<gray>Decides if a shop will
			<gray>reopen at the page a customer
			<gray>had viewed last or at the
			<gray>default page.""")
	public static final Message GUI_SHOP_SET_REMEMBER_PAGE_LORE = new Message("manager.gui.shop.set_remember_page.lore");
	@MessageMeta("<white>Setup Shop Entries")
	public static final Message GUI_SHOP_SET_CONTENT_NAME = new Message("manager.gui.shop.set_content.name");
	@MessageMeta("""
			<gray>Opens the editor for the actual
			<gray>shop pages. Put all the items you
			<gray>want to trade in your lower
			<gray>inventory to have them available
			<gray>in the editor.""")
	public static final Message GUI_SHOP_SET_CONTENT_LORE = new Message("manager.gui.shop.set_content.lore");
	@MessageMeta("<white>Preview This Shop")
	public static final Message GUI_SHOP_SET_PREVIEW_NAME = new Message("manager.gui.shop.preview.name");
	@MessageMeta("<gray>Open this shop menu\n<gray>without enabling it for\n<gray>players.")
	public static final Message GUI_SHOP_SET_PREVIEW_LORE = new Message("manager.gui.shop.preview.lore");
	@MessageGroupMeta(path = "manager.gui.shop.set_default_page", placeholders = {"page", "pages"})
	@MessageMeta("<white>Default Page: <page>")
	public static final Message GUI_SHOP_SET_DEFAULT_PAGE_NAME = new Message("manager.gui.shop.set_default_page.name");
	@MessageMeta("""
			<gray>Current: <gold><page>/<pages>
			<gray>» <yellow>left-click: <gray>Increase
			<gray>» <yellow>right-click: Decrease""")
	public static final Message GUI_SHOP_SET_DEFAULT_PAGE_LORE = new Message("manager.gui.shop.set_default_page.lore");
	@MessageGroupMeta(path = "manager.gui.shop.set_rows", placeholders = "rows")
	@MessageMeta("<white>Menu Size: <rows> Rows")
	public static final Message GUI_SHOP_SET_ROWS_NAME = new Message("manager.gui.shop.set_rows.name");
	@MessageMeta("""
			<gray>Current: <gold><rows>
			<gray>Templates that are already applied
			<gray>will not fit to the lowest row
			<gray>anymore!

			<gray>» <yellow>left-click: <gray>Increase
			<gray>» <yellow>right-click: Decrease""")
	public static final Message GUI_SHOP_SET_ROWS_LORE = new Message("manager.gui.shop.set_rows.lore");
	@MessageMeta("Manage Limits for this Shop")
	public static final Message GUI_SHOP_LIMITS_TITLE = new Message("manager.gui.shop.limits.title");
	@MessageMeta("<gold>Info")
	public static final Message GUI_SHOP_LIMITS_INFO_NAME = new Message("manager.gui.shop.limits.info.name");
	@MessageMeta("""
			<gray>» <yellow>left-click: <gray>add this shops
			<gray>  UUID-tag to the clicked limit
			<gray>» <yellow>right-click: <gray>remove this shops
			<gray>  UUID-tag from the clicked limit""")
	public static final Message GUI_SHOP_LIMITS_INFO_LORE = new Message("manager.gui.shop.limits.info.lore");
	@MessageMeta("Manage Discounts for this Shop")
	public static final Message GUI_SHOP_DISCOUNTS_TITLE = new Message("manager.gui.shop.discounts.title");
	@MessageMeta("<gold>Info")
	public static final Message GUI_SHOP_DISCOUNTS_INFO_NAME = new Message("manager.gui.shop.discounts.info.name");
	@MessageMeta("""
			<gray>» <yellow>left-click: <gray>add this shops
			<gray>UUID-tag to the clicked discount
			<gray>» <yellow>right-click: <gray>remove this shops
			<gray>UUID-tag from the clicked discount""")
	public static final Message GUI_SHOP_DISCOUNTS_INFO_LORE = new Message("manager.gui.shop.discounts.info.lore");
	@MessageMeta("Set the Default Template")
	public static final Message GUI_SHOP_TEMPLATE_TITLE = new Message("manager.gui.shop.template.title");
	@MessageMeta("<gold>Info")
	public static final Message GUI_SHOP_TEMPLATE_INFO_NAME = new Message("manager.gui.shop.template.info.name");
	@MessageMeta("<gray>» <yellow>left-click: <gray>Sets the default Template\n<gray>or resets if already set.")
	public static final Message GUI_SHOP_TEMPLATE_INFO_LORE = new Message("manager.gui.shop.template.info.lore");
	@MessageMeta(value = "Manage Tags for <name>", placeholders = "name")
	public static final Message GUI_TAGS_TITLE = new Message("manager.gui.tags.title");
	@MessageMeta("Add New Tag")
	public static final Message GUI_TAGS_NEW_TAG_TITLE = new Message("manager.gui.tags.new_tag.title");
	@MessageMeta("<white>Create New Tag")
	public static final Message GUI_TAGS_NEW_TAG_NAME = new Message("manager.gui.tags.new_tag.name");
	@MessageMeta("""
			<gray>Click here to create a
			<gray>new tag.
			<gray>Right click tags in the list
			<gray>to delete them.""")
	public static final Message GUI_TAGS_NEW_TAG_LORE = new Message("manager.gui.tags.new_tag.lore");
	@MessageMeta("<gold>Info")
	public static final Message GENERAL_GUI_TAGS_INFO_NAME = new Message("manager.gui.tags.info.name");
	@MessageMeta("""
			<gray>» <yellow>right-click: <gray>delete element

			<gray>If you cannot delete a tag,
			<gray>the tag might be autogenerated or
			<gray>provided by a higher instance.""")
	public static final Message GENERAL_GUI_TAGS_INFO_LORE = new Message("manager.gui.tags.info.lore");
	@MessageMeta(value = "Remove Tag <tag>?", placeholders = "tag")
	public static final Message GENERAL_GUI_TAGS_REMOVE_TAG = new Message("manager.gui.tags.remove_tag");

	@MessageMeta("Enter a page title")
	public static final Message GUI_SHOP_EDITOR_PAGE_TITLE_TITLE = new Message("manager.gui.shop_editor.page_title.title");
	@MessageMeta("<white>Change the page title")
	public static final Message GUI_SHOP_EDITOR_PAGE_TITLE_NAME = new Message("manager.gui.shop_editor.page_title.name");
	@MessageMeta("""
			<gray>Current: <current>
			<gray>Change the display title
			<gray>for the actual page.
			<gray>If not set, an empty string
			<gray>will be inserted.""")
	public static final Message GUI_SHOP_EDITOR_PAGE_TITLE_LORE = new Message("manager.gui.shop_editor.page_title.lore");
	@MessageMeta("<white>Apply Templates to this Page")
	public static final Message GUI_SHOP_EDITOR_APPLY_TEMPLATE_NAME = new Message("manager.gui.shop_editor.template.name");
	@MessageMeta("""
			<gray>You can speed up your
			<gray>work by using shop templates.
			<gray>You can create custom templates
			<gray>and apply them easily to
			<gray>your shop page.

			<gray>» <yellow>left-click: <gray>load template
			<gray>» <yellow>right-click: <gray>save page as template""")
	public static final Message GUI_SHOP_EDITOR_APPLY_TEMPLATE_LORE = new Message("manager.gui.shop_editor.template.lore");
	@MessageMeta("<white>Move items: <value>")
	public static final Message GUI_SHOP_EDITOR_TOGGLE_FREEZE_NAME = new Message("manager.gui.shop_editor.freeze.name");
	@MessageMeta("""
			<gray>You can toggle between
			<gray>Move and Edit Mode.
			<gray>Freeze your shop to edit
			<gray>price, discounts etc.""")
	public static final Message GUI_SHOP_EDITOR_TOGGLE_FREEZE_LORE = new Message("manager.gui.shop_editor.freeze.lore");
	@MessageMeta(value = "<prefix><gray>This shop contains <amount> unused shop entries. Use <white><italic><click:suggest_command:/statshops cleanup >/shops cleanup</click></italic></white> to remove them.",
			placeholders = "amount")
	public static final Message GUI_SHOP_EDITOR_UNUSED_INFO = new Message("manager.gui.shop_editor.unused_items");
	@MessageMeta(value = "<prefix><gray>Removed <white><amount> </white>unused shop entries from <white><shop></white>.",
			placeholders = "amount")
	public static final Message GUI_SHOP_EDITOR_REMOVED_UNUSED = new Message("manager.gui.shop_editor.removed_unused");

	@MessageMeta("<gold>Info")
	public static final Message GUI_VILLAGER_EDITOR_INFO_NAME = new Message("manager.gui.villager_shop_editor.info.name");
	@MessageMeta("<gray>Villager Shops are setup like a\n" +
			"<gray>stack. The trade offer index = row * 9 + column.")
	public static final Message GUI_VILLAGER_EDITOR_INFO_LORE = new Message("manager.gui.villager_shop_editor.info.lore");
	@MessageMeta("<green>New")
	public static final Message GUI_VILLAGER_EDITOR_NEW_NAME = new Message("manager.gui.villager_shop_editor.new.name");
	@MessageMeta("""
			<gray>Take an itemstack from your
			<gray>inventory and click this button
			<gray>to add an offer to the stack.""")
	public static final Message GUI_VILLAGER_EDITOR_NEW_LORE = new Message("manager.gui.villager_shop_editor.new.lore");
	@MessageMeta("<white>Edit")
	public static final Message GUI_VILLAGER_EDITOR_EDIT_NAME = new Message("manager.gui.villager_shop_editor.edit.name");
	@MessageMeta("<gray>Edit the currently selected entry.")
	public static final Message GUI_VILLAGER_EDITOR_EDIT_LORE = new Message("manager.gui.villager_shop_editor.edit.lore");
	@MessageMeta("<gold>Move Left")
	public static final Message GUI_VILLAGER_EDITOR_LEFT_NAME = new Message("manager.gui.villager_shop_editor.left.name");
	@MessageMeta("<gray>Move the currently selected entry\n" +
			"<gray>one offer up in the trader menu.")
	public static final Message GUI_VILLAGER_EDITOR_LEFT_LORE = new Message("manager.gui.villager_shop_editor.left.lore");
	@MessageMeta("<gold>Move Right")
	public static final Message GUI_VILLAGER_EDITOR_RIGHT_NAME = new Message("manager.gui.villager_shop_editor.right.name");
	@MessageMeta("<gray>Move the currently selected entry\n" +
			"<gray>one offer down in the trader menu.")
	public static final Message GUI_VILLAGER_EDITOR_RIGHT_LORE = new Message("manager.gui.villager_shop_editor.right.lore");


	@MessageMeta("Create and Manage Shop Limits")
	public static final Message GUI_LIMITS = new Message("manager.gui.limits.title");
	@MessageMeta(value = "<red>Delete <name>?</red>", placeholders = "name")
	public static final Message GUI_LIMITS_DELETE_CONFIRM = new Message("manager.gui.limits.confirm_delete");
	@MessageMeta(value = "<white><name>", placeholders = "name")
	public static final Message GUI_LIMITS_ENTRY_NAME = new Message("manager.gui.limits.entry.name");
	@MessageMeta(value = """
			<gray>Limit: <yellow><limit>
			<gray>Recover: <yellow><recover>
			<gray>Global: <yellow><global>""",
			placeholders = {"limit", "global", "uuid", "recover"})
	public static final Message GUI_LIMITS_ENTRY_LORE = new Message("manager.gui.limits.entry.lore");
	@MessageMeta("Create New Limit")
	public static final Message GUI_LIMITS_NEW_TITLE = new Message("manager.gui.limits.new.title");
	@MessageMeta("<green>Create New Limit")
	public static final Message GUI_LIMITS_NEW_NAME = new Message("manager.gui.limits.new.name");
	@MessageMeta("""
			<gray>Click to create a new
			<gray>Limit, that can be applied to
			<gray>Shops and ShopEntries.""")
	public static final Message GUI_LIMITS_NEW_LORE = new Message("manager.gui.limits.new.lore");
	@MessageMeta("Change Limit Name")
	public static final Message GUI_LIMIT_SET_NAME_TITLE = new Message("manager.gui.limit.set_name.title");
	@MessageMeta("<white>Set Limit Name")
	public static final Message GUI_LIMIT_SET_NAME_NAME = new Message("manager.gui.limit.set_name.name");
	@MessageMeta("""
			<gray>Click to change the
			<gray>Name of this limit.

			<gray>To <rainbow>format</rainbow> your name
			<gray>use the kyori minimessage format.""")
	public static final Message GUI_LIMIT_SET_NAME_LORE = new Message("manager.gui.limit.set_name.lore");
	@MessageMeta("<white>Open Tag Menu")
	public static final Message GUI_LIMIT_SET_TAGS_NAME = new Message("manager.gui.limit.set_tags.name");
	@MessageMeta("""
			<gray>Discounts and Limits can be
			<gray>applied to all Shops and Shopentries
			<gray>that have certain tags.

			<gray>Use the tags menu to configure
			<gray>tags for this limit.""")
	public static final Message GUI_LIMIT_SET_TAGS_LORE = new Message("manager.gui.limit.set_tags.lore");
	@MessageMeta("Change Limit Permission")
	public static final Message GUI_LIMIT_SET_PERMISSION_TITLE = new Message("manager.gui.limit.set_permission.title");
	@MessageMeta("<white>Set Limit Permission")
	public static final Message GUI_LIMIT_SET_PERMISSION_NAME = new Message("manager.gui.limit.set_permission.name");
	@MessageMeta(value = """
			<gray>Current: <red><permission>

			<gray>Only players with this
			<gray>permission will be affected by
			<gray>this limit.

			<gray>Type "null" to reset.""",
			placeholders = "permission")
	public static final Message GUI_LIMIT_SET_PERMISSION_LORE = new Message("manager.gui.limit.set_permission.lore");
	@MessageMeta("Define a transaction limit")
	public static final Message GUI_LIMIT_SET_LIMIT_TITLE = new Message("manager.gui.limit.set_limit.title");
	@MessageMeta("<white>Transaction Limit")
	public static final Message GUI_LIMIT_SET_LIMIT_NAME = new Message("manager.gui.limit.set_limit.name");
	@MessageMeta(value = """
			<gray>Current:<gold> <current>
			<gray>Define a transaction limit.

			<gray>Customers cannot buy more than
			<gray>the smallest limit of all
			<gray>applied limits.""",
			placeholders = "current")
	public static final Message GUI_LIMIT_SET_LIMIT_LORE = new Message("manager.gui.limit.set_limit.lore");
	@MessageMeta("Define a Cooldown")
	public static final Message GUI_LIMIT_SET_DURATION_TITLE = new Message("manager.gui.limit.set_duration.title");
	@MessageMeta("<white>Cooldown")
	public static final Message GUI_LIMIT_SET_DURATION_NAME = new Message("manager.gui.limit.set_duration.name");
	@MessageMeta(value = """
			<gray>Current:<gold> <current>
			<gray>Every time a customer interacts,
			<gray>the user limit will be increased.
			<gray>After the provided cooldown the
			<gray>customers limit will be
			<gray>decreased.""",
			placeholders = "current")
	public static final Message GUI_LIMIT_SET_DURATION_LORE = new Message("manager.gui.limit.set_duration.lore");
	@MessageMeta("<white>Global")
	public static final Message GUI_LIMIT_SET_GLOBAL_NAME = new Message("manager.gui.limit.set_global.name");
	@MessageMeta(value = """
			<gray>Current: <gold><value>
			<gray>If set to true, all players with
			<gray>the limit permission will share
			<gray>this limit.
			<gray>-> One player can buy all stock
			<gray>items for all players.""",
			placeholders = "value")
	public static final Message GUI_LIMIT_SET_GLOBAL_LORE = new Message("manager.gui.limit.set_global.lore");

	@MessageMeta("Create and Manage Discounts")
	public static final Message GUI_DISCOUNTS = new Message("manager.gui.discounts.title");
	@MessageMeta(value = "<red>Delete <name>?</red>", placeholders = "name")
	public static final Message GUI_DISCOUNTS_DELETE_CONFIRM = new Message("manager.gui.discounts.confirm_delete");
	@MessageGroupMeta(path = "manager.gui.discounts.entry", comment = "Defines, how each discount will be displayed in the list")
	public static final Message GUI_DISCOUNTS_ENTRY_NAME = new Message("manager.gui.discounts.entry.name");
	public static final Message GUI_DISCOUNTS_ENTRY_LORE = new Message("manager.gui.discounts.entry.lore");
	@MessageMeta("Create a new discount")
	public static final Message GUI_DISCOUNTS_NEW_TITLE = new Message("manager.gui.discounts.new.title");
	@MessageMeta("<green>Create New Discount")
	public static final Message GUI_DISCOUNTS_NEW_NAME = new Message("manager.gui.discounts.new.name");
	@MessageMeta("<gray>Click to create a \n<gray>new discount.")
	public static final Message GUI_DISCOUNTS_NEW_LORE = new Message("manager.gui.discounts.new.lore");

	@MessageMeta("Edit Discount")
	public static final Message GUI_DISCOUNT = new Message("manager.gui.discount.title");
	@MessageMeta("Set Discount Name")
	public static final Message GUI_DISCOUNT_SET_NAME_TITLE = new Message("manager.gui.discount.set_name.title");
	@MessageMeta("<white>Set Name")
	public static final Message GUI_DISCOUNT_SET_NAME_NAME = new Message("manager.gui.discount.set_name.name");
	@MessageMeta("""
			<gray>Click to change the
			<gray>name of the discount.

			<gray>To <rainbow>format</rainbow> your name
			<gray>use the kyori minimessage format""")
	public static final Message GUI_DISCOUNT_SET_NAME_LORE = new Message("manager.gui.discount.set_name.lore");
	@MessageMeta("Set Discount Permission")
	public static final Message GUI_DISCOUNT_SET_PERMISSION_TITLE = new Message("manager.gui.discount.set_permission.title");
	@MessageMeta("<white>Set Permission")
	public static final Message GUI_DISCOUNT_SET_PERMISSION_NAME = new Message("manager.gui.discount.set_permission.name");
	@MessageMeta(value = """
			<gray>Current: <red><permission>

			<gray>Only players with this
			<gray>permission will be able to
			<gray>use this discount.

			<gray>Type "null" to reset.""",
			placeholders = "permission")
	public static final Message GUI_DISCOUNT_SET_PERMISSION_LORE = new Message("manager.gui.discount.set_permission.lore");
	@MessageMeta("<white>Open Tag Menu")
	public static final Message GUI_DISCOUNT_SET_TAGS_NAME = new Message("manager.gui.discount.set_tags.name");
	@MessageMeta("""
			<gray>Apply this discount to all
			<gray>shops and shopentries with
			<gray>certain tags like "swords" or "food".""")
	public static final Message GUI_DISCOUNT_SET_TAGS_LORE = new Message("manager.gui.discount.set_tags.lore");
	@MessageMeta("Set a Duration")
	public static final Message GUI_DISCOUNT_SET_DURATION_TITLE = new Message("manager.gui.discount.set_duration.title");
	@MessageMeta("<white>Set Duration")
	public static final Message GUI_DISCOUNT_SET_DURATION_NAME = new Message("manager.gui.discount.set_duration.name");
	@MessageMeta(value = """
			<gray>Current: <gold><duration>

			<gray>Set the duration the discount
			<gray>will be active for.

			<red><bold>!</bold></red><gray> If your provided duration is too long
			<gray>and start dates would overlap
			<gray>the action will be cancelled!""",
			placeholders = "duration")
	public static final Message GUI_DISCOUNT_SET_DURATION_LORE = new Message("manager.gui.discount.set_duration.lore");
	@MessageMeta("Add New Start Dates")
	public static final Message GUI_DISCOUNT_SET_START_TITLE = new Message("manager.gui.discount.set_start.title");
	@MessageMeta("<white>Start Dates")
	public static final Message GUI_DISCOUNT_SET_START_NAME = new Message("manager.gui.discount.set_start.name");
	@MessageMeta("""
			<gray>You can set many different
			<gray>start dates for one discount.
			<gray>Every start date has to be
			<gray>at least the duration apart
			<gray>from the next and previous.""")
	public static final Message GUI_DISCOUNT_SET_START_LORE = new Message("manager.gui.discount.set_start.lore");
	@MessageMeta("Set a Percentage")
	public static final Message GUI_DISCOUNT_SET_PERCENT_TITLE = new Message("manager.gui.discount.set_percent.title");
	@MessageMeta("<white>Set Percent")
	public static final Message GUI_DISCOUNT_SET_PERCENT_NAME = new Message("manager.gui.discount.set_percent.name");
	@MessageMeta(value = """
			<gray>Current: <gold><percent>%

			<gray>Sets the percentage to multiply with the pirce.
			<gray>50% -> The item costs 1.5x as much.
			<gray>-50% -> The item costs half as much.""", placeholders = "percent")
	public static final Message GUI_DISCOUNT_SET_PERCENT_LORE = new Message("manager.gui.discount.set_percent.lore");
	@MessageMeta("<gold>Info")
	public static final Message GUI_DISCOUNT_START_INFO_NAME = new Message("manager.gui.discount.start.info.name");
	@MessageMeta("""
			<gray>You can create multiple start dates
			<gray>for your discount. The discount will
			<gray>start at every start date for
			<gray>the given duration.

			<gray>If you want varying durations, you
			<gray>have to create multiple discounts.

			<gray>» <yellow>right-click: <gray>delete element""")
	public static final Message GUI_DISCOUNT_START_INFO_LORE = new Message("manager.gui.discount.start.info.lore");
	@MessageMeta("Enter a Start Date")
	public static final Message GUI_DISCOUNT_START_NEW_TITLE = new Message("manager.gui.discount.start.new.title");
	@MessageMeta("<green>New Start Date")
	public static final Message GUI_DISCOUNT_START_NEW_NAME = new Message("manager.gui.discount.start.new.name");
	@MessageMeta("<gray>Click to add a\n<gray>start date.")
	public static final Message GUI_DISCOUNT_START_NEW_LORE = new Message("manager.gui.discount.start.new.lore");
	@MessageMeta(value = "<red>Delete start-date <date>?</red>", placeholders = "date")
	public static final Message GUI_DISCOUNT_START_DELETE_CONFIRM = new Message("manager.gui.discount.start.confirm_delete");

	public static final Message GUI_SHOP_ENTRY = new Message("manager.gui.shop_entry.title");

	@MessageMeta("Choose a Template")
	public static final Message GUI_TEMPLATES_CHOOSE = new Message("manager.gui.templates.choose_title");
	@MessageMeta(value = "Apply template <name>?", placeholders = "name")
	public static final Message GUI_TEMPLATES_APPLY = new Message("manager.gui.templates.apply_title");
	@MessageMeta("Save as template?")
	public static final Message GUI_TEMPLATES_NEW = new Message("manager.gui.templates.new_title");
	@MessageMeta(value = "<template>", placeholders = "template")
	public static final Message GUI_TEMPLATES_ENTRY_NAME = new Message("manager.gui.templates.entry.name");
	@MessageMeta(value = "<gray>Size: <size>", placeholders = "size")
	public static final Message GUI_TEMPLATES_ENTRY_LORE = new Message("manager.gui.templates.entry.lore");

	@MessageMeta
	public static final Message GUI_ENTRY_TITLE = new Message("manager.gui.entry.title");
	@MessageMeta("<white>Set Info Lore")
	public static final Message GUI_ENTRY_SET_LORE_NAME = new Message("manager.gui.entry.set_lore.name");
	@MessageMeta("""
			<gray>Click to change the
			<gray>description of the entry.

			<gray>This is NOT similar to setting a
			<gray>lore for your item beforehand.
			<gray>This lore will not be on the
			<gray>purchased Item.

			<gray>To <rainbow>format</rainbow> your name
			<gray>use the Kyori MiniMessage format""")
	public static final Message GUI_ENTRY_SET_LORE_LORE = new Message("manager.gui.entry.set_lore.lore");
	@MessageMeta("Change Entry Permission")
	public static final Message GUI_ENTRY_SET_PERMISSION_TITLE = new Message("manager.gui.entry.set_permission.title");
	@MessageMeta("<white>Set Entry Permission")
	public static final Message GUI_ENTRY_SET_PERMISSION_NAME = new Message("manager.gui.entry.set_permission.name");
	@MessageMeta(value = """
			<gray>Current: <red><permission>

			<gray>Only players with this\s
			<gray>permission will be able to\s
			<gray>use this entry.

			<gray>Type "null" to reset.""", placeholders = "permission")
	public static final Message GUI_ENTRY_SET_PERMISSION_LORE = new Message("manager.gui.entry.set_permission.lore");
	@MessageMeta("<white>Open Tag Menu")
	public static final Message GUI_ENTRY_SET_TAGS_NAME = new Message("manager.gui.entry.set_tags.name");
	@MessageMeta("""
			<gray>Discounts and Limits can be
			<gray>applied to all Shops and Shopentries
			<gray>that have certain tags.

			<gray>Use the tags menu to configure
			<gray>tags for this entry.""")
	public static final Message GUI_ENTRY_SET_TAGS_LORE = new Message("manager.gui.entry.set_tags.lore");
	@MessageMeta("Choose an Entry Function")
	public static final Message GUI_ENTRY_SET_FUNCTION_TITLE = new Message("manager.gui.entry.set_function.title");
	@MessageMeta(value = "<name>", placeholders = "name")
	public static final Message GUI_ENTRY_SET_FUNCTION_NAME = new Message("manager.gui.entry.set_function.name");
	@MessageMeta(value = """
			<gray>Click to choose a function
			<gray>for this Shop Entry.

			<gray>Current: <function>""", placeholders = "function")
	public static final Message GUI_ENTRY_SET_FUNCTION_LORE = new Message("manager.gui.entry.set_function.lore");
	@MessageMeta("Choose Costs")
	public static final Message GUI_ENTRY_SET_COSTS_TITLE = new Message("manager.gui.entry.set_costs.title");

	@MessageMeta("<white>Static Item")
	public static final Message GUI_ENTRY_FUNCTION_STATIC_NAME = new Message("manager.gui.entry.defaults.static.name");
	@MessageMeta("<gray>Simply does nothing.")
	public static final Message GUI_ENTRY_FUNCTION_STATIC_LORE = new Message("manager.gui.entry.defaults.static.lore");
	@MessageMeta("<white>Close Shop")
	public static final Message GUI_ENTRY_FUNCTION_CLOSE_NAME = new Message("manager.gui.entry.defaults.close.name");
	@MessageMeta("<gray>Closes the shop window.")
	public static final Message GUI_ENTRY_FUNCTION_CLOSE_LORE = new Message("manager.gui.entry.defaults.close.lore");
	@MessageMeta("<white>Open Shop")
	public static final Message GUI_ENTRY_FUNCTION_OPEN_SHOP_NAME = new Message("manager.gui.entry.defaults.open_shop.name");
	@MessageMeta("<gray>Opens another shop.")
	public static final Message GUI_ENTRY_FUNCTION_OPEN_SHOP_LORE = new Message("manager.gui.entry.defaults.open_shop.lore");
	@MessageMeta("<white>Previous Page")
	public static final Message GUI_ENTRY_FUNCTION_PREV_PAGE_NAME = new Message("manager.gui.entry.defaults.prev_page.name");
	@MessageMeta("<gray>Turns n pages back.")
	public static final Message GUI_ENTRY_FUNCTION_PREV_PAGE_LORE = new Message("manager.gui.entry.defaults.prev_page.lore");
	@MessageMeta("<white>Next Page")
	public static final Message GUI_ENTRY_FUNCTION_NEXT_PAGE_NAME = new Message("manager.gui.entry.defaults.next_page.name");
	@MessageMeta("<gray>Turns n pages ahead.")
	public static final Message GUI_ENTRY_FUNCTION_NEXT_PAGE_LORE = new Message("manager.gui.entry.defaults.next_page.lore");
	@MessageMeta("<white>Exact Page")
	public static final Message GUI_ENTRY_FUNCTION_EXACT_PAGE_NAME = new Message("manager.gui.entry.defaults.exact_page.name");
	@MessageMeta("<gray>Opens an exact page.")
	public static final Message GUI_ENTRY_FUNCTION_EXACT_PAGE_LORE = new Message("manager.gui.entry.defaults.exact_page.lore");
	@MessageMeta("<white>Buy Item")
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_ITEM_NAME = new Message("manager.gui.entry.defaults.article_item.name");
	@MessageMeta("<gray>Simply buy items.")
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_ITEM_LORE = new Message("manager.gui.entry.defaults.article_item.lore");
	@MessageMeta("<white>Buy Commands")
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_CMD_NAME = new Message("manager.gui.entry.defaults.article_cmd.name");
	@MessageMeta("<gray>Buy the execution of a command.")
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_CMD_LORE = new Message("manager.gui.entry.defaults.article_cmd.lore");
	@MessageMeta("<white>Buy Console Commands")
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_CONSOLE_CMD_NAME = new Message("manager.gui.entry.defaults.article_console_cmd.name");
	@MessageMeta("<gray>Buy the execution of a command\n<gray>that is executed in console.")
	public static final Message GUI_ENTRY_FUNCTION_ARTICLE_CONSOLE_CMD_LORE = new Message("manager.gui.entry.defaults.article_console_cmd.lore");
	@MessageMeta("<white>Item Costs")
	public static final Message GUI_ENTRY_FUNCTION_COSTS_ITEM_NAME = new Message("manager.gui.entry.defaults.costs_item.name");
	@MessageMeta("<gray>Pay items to buy articles.")
	public static final Message GUI_ENTRY_FUNCTION_COSTS_ITEM_LORE = new Message("manager.gui.entry.defaults.costs_item.lore");
	@MessageMeta("<white>XP Costs")
	public static final Message GUI_ENTRY_FUNCTION_COSTS_XP_NAME = new Message("manager.gui.entry.defaults.costs_xp.name");
	@MessageMeta("<gray>Pay XP to buy articles.")
	public static final Message GUI_ENTRY_FUNCTION_COSTS_XP_LORE = new Message("manager.gui.entry.defaults.costs_xp.lore");

	@MessageGroupMeta(path = "manager.gui.entry.function.type", placeholders = "name")
	@MessageMeta("<white>Boolean: <name>")
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_BOOL = new Message("manager.gui.entry.function.type.bool");
	@MessageMeta("<white>Number: <name>")
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_INTEGER = new Message("manager.gui.entry.function.type.int");
	@MessageMeta("<white>Term: <name>")
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_EQUATION = new Message("manager.gui.entry.function.type.equation");
	@MessageMeta("<white>Text: <name>")
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_STRING = new Message("manager.gui.entry.function.type.string");
	@MessageMeta("<white>Shop: <name>")
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_SHOP = new Message("manager.gui.entry.function.type.shop");
	@MessageMeta("<white>Item: <name>")
	public static final Message GUI_ENTRY_FUNCTION_DATA_TYPE_ITEMSTACK = new Message("manager.gui.entry.function.type.itemstack");

	@MessageGroupMeta(path = "manager.gui.entry.function.data", placeholders = "current")
	@MessageMeta("Open Shop")
	public static final Message GUI_ENTRY_FUNCTION_OPENED_SHOP_NAME = new Message("manager.gui.entry.function.data.open_shop.name");
	@MessageMeta("<gray>Current: <gold><current>\n<gray>The shop to open when interacted.")
	public static final Message GUI_ENTRY_FUNCTION_OPENED_SHOP_LORE = new Message("manager.gui.entry.function.data.open_shop.lore");
	@MessageMeta("Page")
	public static final Message GUI_ENTRY_FUNCTION_PAGE_NAME = new Message("manager.gui.entry.function.data.page.name");
	@MessageMeta("<gray>Current: <gold><current>\n<gray>The amount of pages.")
	public static final Message GUI_ENTRY_FUNCTION_PAGE_LORE = new Message("manager.gui.entry.function.data.page.lore");
	@MessageMeta("Purchasable")
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_NAME = new Message("manager.gui.entry.function.data.purchasable.name");
	@MessageMeta("<gray>If this entry can be purchased.")
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_LORE = new Message("manager.gui.entry.function.data.purchasable.lore");
	@MessageMeta("Purchasable stacked")
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_NAME = new Message("manager.gui.entry.function.data.purchasable_stacked.name");
	@MessageMeta("<gray>If this entry can be purchased\n<gray>in stacks.")
	public static final Message GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_LORE = new Message("manager.gui.entry.function.data.purchasable_stacked.lore");
	@MessageMeta("Sellable")
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_NAME = new Message("manager.gui.entry.function.data.sellable.name");
	@MessageMeta("<gray>If customers can sell items on\n<gray>this entry.")
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_LORE = new Message("manager.gui.entry.function.data.sellable.lore");
	@MessageMeta("Sellable stacked")
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_STACKED_NAME = new Message("manager.gui.entry.function.data.sellable_stacked.name");
	@MessageMeta("<gray>If customers can sell items as\n<gray>stacks on this entry.")
	public static final Message GUI_ENTRY_FUNCTION_SELLABLE_STACKED_LORE = new Message("manager.gui.entry.function.data.sellable_stacked.lore");
	@MessageMeta("Article Amount")
	public static final Message GUI_ENTRY_FUNCTION_GAIN_AMOUNT_NAME = new Message("manager.gui.entry.function.data.gain_amount.name");
	@MessageMeta("""
			<gray>Current: <gold><current>
			<gray>The amount of articles to sell in
			<gray>one interaction with the entry.
			<gray>Most likely 1.""")
	public static final Message GUI_ENTRY_FUNCTION_GAIN_AMOUNT_LORE = new Message("manager.gui.entry.function.data.gain_amount.lore");
	@MessageMeta("Buy Price Item")
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_NAME = new Message("manager.gui.entry.function.data.buy_price_amount.name");
	@MessageMeta("<gray>The item that the customer\n<gray>pays to get the article.")
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_LORE = new Message("manager.gui.entry.function.data.buy_price_amount.lore");
	@MessageMeta("Sell Price Item")
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_NAME = new Message("manager.gui.entry.function.data.sell_price_amount.name");
	@MessageMeta("""
			<gray>The item that the customer
			<gray>gets when selling the article.

			<gray>Only necessary when "sellable"
			<gray>is active.""")
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_LORE = new Message("manager.gui.entry.function.data.sell_price_amount.lore");
	@MessageMeta("Buy Price Equation")
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_NAME = new Message("manager.gui.entry.function.data.buy_price_equation.name");
	@MessageMeta("""
			<gray>Current: <gold><current>
			<gray>The amount of pay items to
			<gray>pay to get the article.

			<gray>You may want to use an external
			<gray>database for prices. Reference
			<gray>entries with <white><db:diamond></white> for example.""")
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_LORE = new Message("manager.gui.entry.function.data.buy_price_equation.lore");
	@MessageMeta("Sell Price Equation")
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_NAME = new Message("manager.gui.entry.function.data.sell_price_equation.name");
	@MessageMeta("""
			<gray>Current: <gold><current>
			<gray>The amount of sell price items
			<gray>that the customer gets when selling
			<gray>the article.

			<gray>You may want to use an external
			<gray>database for prices. Reference
			<gray>entries with <white><db:diamond></white> for example.""")
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_LORE = new Message("manager.gui.entry.function.data.sell_price_equation.lore");
	@MessageMeta("Article")
	public static final Message GUI_ENTRY_FUNCTION_GAIN_ITEM_NAME = new Message("manager.gui.entry.function.data.gain_item.name");
	@MessageMeta("<gray>The item that will be sold\n<gray>to the customer.")
	public static final Message GUI_ENTRY_FUNCTION_GAIN_ITEM_LORE = new Message("manager.gui.entry.function.data.gain_item.lore");
	@MessageMeta("Buy Price Item")
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_NAME = new Message("manager.gui.entry.function.data.buy_price_item.name");
	@MessageMeta("<gray>The item that the customer\n<gray>pays to get the article.")
	public static final Message GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_LORE = new Message("manager.gui.entry.function.data.buy_price_item.lore");
	@MessageMeta("Sell Price Item")
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_NAME = new Message("manager.gui.entry.function.data.sell_price_item.name");
	@MessageMeta("""
			<gray>The item that the customer
			<gray>gets when selling the article.

			<gray>Only necessary when "sellable"
			<gray>is active.""")
	public static final Message GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_LORE = new Message("manager.gui.entry.function.data.sell_price_item.lore");
	@MessageMeta("Command")
	public static final Message GUI_ENTRY_FUNCTION_COMMAND_NAME = new Message("manager.gui.entry.function.data.command.name");
	@MessageMeta("""
			<gray>Current: <gold><current>
			<gray>The command to execute. Use <player>
			<gray>to reference the customer.""")
	public static final Message GUI_ENTRY_FUNCTION_COMMAND_LORE = new Message("manager.gui.entry.function.data.command.lore");

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
