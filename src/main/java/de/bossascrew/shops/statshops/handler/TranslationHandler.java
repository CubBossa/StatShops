package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LoadedMessage;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

public class TranslationHandler implements WebAccessable<LoadedMessage> {

	@Getter
	private static TranslationHandler instance;

	private String activeLanguage = "none";
	private final Map<String, String> messageFormats;
	private final Map<String, String> fallbackLanguage;

	public TranslationHandler(String startlanguage) {
		instance = this;
		messageFormats = new HashMap<>();
		loadLanguage("en_US");
		fallbackLanguage = new HashMap<>(messageFormats);
		loadLanguage(startlanguage);
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

	public String getMessage(String key) {
		if (StatShops.getInstance().getShopsConfig().isLanguageUseFallback()) {
			return messageFormats.getOrDefault(key, fallbackLanguage.getOrDefault(key, activeLanguage + "-missing:" + key));
		}
		return messageFormats.getOrDefault(key, activeLanguage + "-missing:" + key);
	}

	@Override
	public List<LoadedMessage> getWebData() {
		List<LoadedMessage> messages = new ArrayList<>();
		for (Message message : Message.values()) {
			messages.add(new LoadedMessage(activeLanguage, message.getKey(), getMessage(message.getKey()), message.getComment(), message.getExamplePlaceholders()));
		}
		return messages;
	}

	@Override
	public void storeWebData(List<LoadedMessage> values) {
		Map<String, List<LoadedMessage>> messagesPerLanguage = new HashMap<>();
		for (LoadedMessage m : values) {
			List<LoadedMessage> messages = messagesPerLanguage.getOrDefault(m.getLanguageKey(), new ArrayList<>());
			messages.add(m);
			messagesPerLanguage.put(m.getLanguageKey(), messages);
		}
		for (Map.Entry<String, List<LoadedMessage>> entry : messagesPerLanguage.entrySet()) {
			File file = new File(StatShops.getInstance().getDataFolder(), "lang/" + entry.getKey() + ".yml");
			try {
				if (file.createNewFile()) {
					StatShops.getInstance().log(LoggingPolicy.INFO, "Created new language file: lang/" + entry.getKey() + ".yml");
				}
			} catch (IOException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Error while editing language file: lang/" + entry.getKey() + ".yml", e);
				continue;
			}
			YamlConfiguration cfg = YamlConfiguration.loadConfiguration(file);
			for (LoadedMessage message : entry.getValue()) {
				cfg.set(message.getKey(), message.getValue());
			}
			try {
				cfg.save(file);
			} catch (IOException e) {
				StatShops.getInstance().log(LoggingPolicy.ERROR, "Error while saving to language file: lang/" + entry.getKey() + ".yml", e);
			}

			if (entry.getKey().equalsIgnoreCase(activeLanguage)) {
				for (LoadedMessage message : entry.getValue()) {
					messageFormats.put(message.getKey(), message.getValue());
				}
			}
		}
	}
}
