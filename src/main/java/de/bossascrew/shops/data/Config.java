package de.bossascrew.shops.data;

import de.bossascrew.shops.data.config.AnnotationConfig;
import de.bossascrew.shops.data.config.ConfigEntry;
import de.bossascrew.shops.data.config.ConfigFile;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.Material;

import java.io.File;

@Getter
@Setter
@ConfigFile(header = """
		##########################
		#       Statshops        #
		# by CubBossa & JannisGo #
		##########################
		  
		""")
public class Config extends AnnotationConfig {

	@ConfigEntry(name = "general.language", comment = "Set the default language.")
	public String language = "en_US";
	/*@ConfigEntry(name = "general.logging_policy", comment = """
			Sets the minimum logging policy.
			- 'DEBUG' logs many information
			- 'INFO' logs default information
			- 'WARN' only shows warnings
			- 'ERROR' only shows errors""")*/
	public LoggingPolicy loggingPolicy = LoggingPolicy.INFO;

	@ConfigEntry(name = "shops.buy_icon", comment = "Set the display material for the 'buy' mode.")
	public Material shopBuyIconMaterial = Material.DIAMOND;
	@ConfigEntry(name = "shops.sell_icon", comment = "Set the display material for the 'sell' mode.")
	public Material shopSellIconMaterial = Material.GOLD_INGOT;
	@ConfigEntry(name = "shops.trade_icon", comment = "Set the display material for the 'trade' mode.")
	public Material shopTradeIconMaterial = Material.EMERALD;

	@ConfigEntry(name = "gui.confirm_general_deletion", comment = "Sets if you have to confirm the deletion of shops/discounts/limits.")
	public boolean confirmDeletion = false;
	@ConfigEntry(name = "gui.confirm_tag_deletion", comment = "Sets if you have to confirm the deletion of tags.")
	public boolean confirmTagDeletion = false;


	public Config(String path) {
		super(path);

		File file = new File(path);
		if (!file.exists()) {
			saveConfig();
		}
	}
}
