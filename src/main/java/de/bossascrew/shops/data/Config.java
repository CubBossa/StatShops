package de.bossascrew.shops.data;

import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.Material;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.event.inventory.ClickType;

import java.time.Duration;

@Getter
@Setter
public class Config {


	YamlConfiguration cfg;

	public static final String CONF_LANGUAGE = "language";
	public static final String CONF_LOGGING = "logging";
	public static final String CONF_KEY_BUY = "defaults.keybinding.buy";


	private boolean logTransactions = true;
	private Duration transactionLogExpire = null;


	private String language = "en_US";
	private LoggingPolicy loggingPolicy = LoggingPolicy.INFO;

	private Material shopBuyIconMaterial = Material.DIAMOND;
	private Material shopSellIconMaterial = Material.GOLD_INGOT;
	private Material shopTradeIconMaterial = Material.EMERALD;

	private boolean confirmTagDeletion = false;

	public Config(String fileName) {

	}

	public void reloadLanguage() {

	}






}
