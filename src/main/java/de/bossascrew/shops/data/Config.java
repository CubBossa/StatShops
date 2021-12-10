package de.bossascrew.shops.data;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.config.AnnotationConfig;
import de.bossascrew.shops.data.config.ConfigEntry;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;

import java.io.File;

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
	//TODO Shopmode

	//TAGS
	@ConfigEntry(path = "tags.auto-tagging-materials")
	public boolean autoTaggingMaterials = false;
	@ConfigEntry(path = "tags.auto-tagging-groups")
	public boolean autoTaggingGroups = false;


	public Config(String path) {
		super(path);

		File file = new File(path);
		if (!file.exists()) {
			ShopPlugin.getInstance().saveResource("config.yml", true);
		}
	}
}
