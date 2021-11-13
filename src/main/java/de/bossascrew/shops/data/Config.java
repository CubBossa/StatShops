package de.bossascrew.shops.data;

import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.Material;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.inventory.ClickType;

@Getter
@Setter
public class Config {


	YamlConfiguration cfg;

	public static final String CONF_LANGUAGE = "language";
	public static final String CONF_LOGGING = "logging";
	public static final String CONF_KEY_BUY = "defaults.keybinding.buy";


	private String language = "en_US";
	private LoggingPolicy loggingPolicy = LoggingPolicy.INFO;


	private ClickType keyBindDelete = ClickType.RIGHT;

	private Material shopBuyIconMaterial;
	private Material shopSellIconMaterial;



	public Config(String fileName) {

	}

	public void reloadLanguage() {

	}






}
