package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.config.AnnotationConfig;
import de.bossascrew.shops.general.config.ConfigEntry;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import lombok.Getter;
import lombok.Setter;

import java.io.File;
import java.util.ArrayList;
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
	@ConfigEntry(path = "general.base-command")
	public List<String> baseCommands = new ArrayList<>();


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
	//TODO Shopmode

	@ConfigEntry(path = "shops.default-template")
	public String defaultTemplate = "000000000-0000-0000-00000-0000000000001";

	//TAGS
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
