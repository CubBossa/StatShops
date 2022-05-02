package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class TranslationHandler {

	@Getter
	private static TranslationHandler instance;

	private String activeLanguage = "none";
	private final Map<String, Message> messageMap;
	private final Map<String, String> messageFormats;
	private final Map<String, String> fallbackLanguage;

	public TranslationHandler(String startlanguage) {
		instance = this;
		messageMap = new HashMap<>();
		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.registerMessages(this);
		}
		messageMap.putAll(Message.values().stream().collect(Collectors.toMap(Message::getKey, message -> message)));
		messageFormats = new HashMap<>();
		loadLanguage("en_US");
		fallbackLanguage = new HashMap<>(messageFormats);
		loadLanguage(startlanguage);

	}

	public void registerMessages(Message... messages) {
		messageMap.putAll(Arrays.stream(messages).collect(Collectors.toMap(Message::getKey, message -> message)));
	}

	public CompletableFuture<Boolean> loadLanguage(String languageKey) {
		activeLanguage = languageKey;
		messageFormats.clear();

		return CompletableFuture.supplyAsync(() -> {

			File file = new File(StatShops.getInstance().getDataFolder(), "lang/" + languageKey + ".yml");
			if (!file.exists()) {
				StatShops.getInstance().saveResource("lang/" + languageKey + ".yml", false);
				file = new File(StatShops.getInstance().getDataFolder(), "lang/" + languageKey + ".yml");
				if (!file.exists()) {

					StatShops.getInstance().log(LoggingPolicy.ERROR, "Error while creating language.yml for " + languageKey);
					return false;
				}
			}
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);
			Map<String, Object> map = cfg.getValues(true);
			for (Map.Entry<String, Object> entry : map.entrySet()) {
				if (entry.getValue() instanceof String s) {
					messageFormats.put(entry.getKey(), s);
				}
			}
			return true;
		});
	}

	public Message getMessage(String key) {
		return messageMap.getOrDefault(key, Message.NONE);
	}

	public String getTranslation(String key) {
		if (StatShops.getInstance().getShopsConfig().isLanguageUseFallback()) {
			return messageFormats.getOrDefault(key, fallbackLanguage.getOrDefault(key, activeLanguage + "-missing:" + key));
		}
		return messageFormats.getOrDefault(key, activeLanguage + "-missing:" + key);
	}
}
