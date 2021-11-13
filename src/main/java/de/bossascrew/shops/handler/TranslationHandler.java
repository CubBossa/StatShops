package de.bossascrew.shops.handler;

import lombok.Getter;

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



	}

	public String getMessage(String key) {
		return messageFormats.getOrDefault(key, activeLanguage + "-missing:" + key);
	}
}
