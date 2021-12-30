package de.bossascrew.shops.statshops.data;

import com.google.common.collect.Lists;
import de.bossascrew.shops.general.config.AnnotationConfig;
import de.bossascrew.shops.general.config.ConfigEntry;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.TradeMessageType;
import de.bossascrew.shops.statshops.StatShops;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.inventory.ClickType;

import java.io.File;
import java.util.List;

@Getter
@Setter
public class Config extends AnnotationConfig {

	//GENERAL
	@ConfigEntry(path = "general.language")
	public String language = "en-US";
	@ConfigEntry(path = "general.use-fallback-language")
	public boolean languageUseFallback = false;
	//@ConfigEntry(path = "general.logging-policy")
	public LoggingPolicy loggingPolicy = LoggingPolicy.INFO;
	@ConfigEntry(path = "general.message-caching")
	public int messageCaching = 500;

	//GUIS
	@ConfigEntry(path = "guis.confirm-general-deletion")
	public boolean confirmDeletion = false;
	@ConfigEntry(path = "gui.confirm-tag-deletion")
	public boolean confirmTagDeletion = false;


	//SHOPS
	//TODO keybinding
	@ConfigEntry(path = "shops.cooldown")
	public int cooldown = 100;
	@ConfigEntry(path = "shops.cooldown-message")
	public boolean showCooldownMessage = true;
	@ConfigEntry(path = "shops.shop-size")
	public int defaultShopSize = 3;

	@ConfigEntry(path = "shops.keybinding.buy")
	public List<String> buyKeyBinding = List.of(ClickType.LEFT.toString());
	@ConfigEntry(path = "shops.keybinding.sell")
	public List<String> sellKeyBinding = List.of(ClickType.RIGHT.toString());
	@ConfigEntry(path = "shops.keybinding.buy-stack")
	public List<String> buyStackKeyBinding = List.of(ClickType.SHIFT_LEFT.toString());
	@ConfigEntry(path = "shops.keybinding.sell-stack")
	public List<String> sellStackKeyBinding = List.of(ClickType.SHIFT_RIGHT.toString());

	@ConfigEntry(path = "shops.default-template")
	public String defaultTemplate = "000000000-0000-0000-00000-0000000000001";

	@ConfigEntry(path = "shops.trade-message-feedback")
	public TradeMessageType tradeMessageFeedback = TradeMessageType.CUMULATIVE_SHOP;

	@ConfigEntry(path = "shops.entry-lore")
	public List<String> entryLoreOrder = Lists.newArrayList("price", "actions", "discounts", "limits", "info");

	//CURRENCIES
	@ConfigEntry(path = "currencies.item-formatting")
	public String currencyItemFormatting = "<white><amount>x</white> <gray><currency></gray>";
	@ConfigEntry(path = "currencies.item-formatting-discounted")
	public String currencyItemFormattingDiscounted = "<white><st><amount></st> <amount_dc><white> <gray><currency></gray>";
	@ConfigEntry(path = "currencies.vault-formatting")
	public String currencyVaultFormatting = "<yellow><amount></yellow> <gold><currency></gold>";
	@ConfigEntry(path = "currencies.vault-formatting-discounted")
	public String currencyVaultFormattingDiscounted = "<yellow><st><amount></st> <amount_dc></yellow> <gold><currency></gold>";

	@ConfigEntry(path = "currencies.dynamic.enabled")
	public boolean dynamicPricingEnabled = false;
	@ConfigEntry(path = "currencies.dynamic.live_update")
	public boolean dynamicPricingLiveEnabled = false;
	@ConfigEntry(path = "currencies.dynamic.load_intern_defaults")
	public boolean dynamicPricingInternalData = true;
	@ConfigEntry(path = "currencies.dynamic.load_essentials_defaults")
	public boolean dynamicPricingEssentialsData = false;


	//TAGS
	@ConfigEntry(path = "tags.auto-tagging-currency")
	public boolean autoTaggingCurrency = false;
	@ConfigEntry(path = "tags.auto-tagging-article")
	public boolean autoTaggingArticle = false;
	@ConfigEntry(path = "tags.auto-tagging-materials")
	public boolean autoTaggingMaterials = false;
	@ConfigEntry(path = "tags.auto-tagging-groups")
	public boolean autoTaggingGroups = false;
	@ConfigEntry(path = "tags.auto-tagging-enchants")
	public boolean autoTaggingEnchantments = false;
	@ConfigEntry(path = "tags.auto-tagging-potions")
	public boolean autoTaggingPotions = false;
	@ConfigEntry(path = "tags.auto-tagging-attributes")
	public boolean autoTaggingAttributes = false;


	public Config(String path) {
		super(path);

		File file = new File(path);
		if (!file.exists()) {
			StatShops.getInstance().saveResource("config.yml", true);
		}
	}
}
