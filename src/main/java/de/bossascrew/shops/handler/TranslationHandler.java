package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

public class TranslationHandler {

	@Getter
	private static TranslationHandler instance;

	private String activeLanguage = "none";
	private final Map<String, String> messageFormats;

	public TranslationHandler(String startlanguage) {
		instance = this;
		messageFormats = new HashMap<>();
		loadLanguage(startlanguage);
	}

	public void loadLanguage(String languageKey) {
		activeLanguage = languageKey;
		messageFormats.clear();

		File file = new File(ShopPlugin.getInstance().getDataFolder(), "lang/" + languageKey + ".yml");
		if (!file.exists()) {
			ShopPlugin.getInstance().saveResource("lang/" + languageKey + ".yml", false);
			file = new File(ShopPlugin.getInstance().getDataFolder(), "lang/" + languageKey + ".yml");
			if (!file.exists()) {

				ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Error while creating language.yml for " + languageKey);
				return;
			}
		}
		YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);
		Map<String, Object> map = cfg.getValues(true);
		for (Map.Entry<String, Object> entry : map.entrySet()) {
			if (entry.getValue() instanceof String s) {
				messageFormats.put(entry.getKey(), s);
			}
		}
	}

	public String getMessage(String key) {
		return messageFormats.getOrDefault(key, activeLanguage + "-missing:" + key);
	}
}
