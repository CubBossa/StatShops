package de.bossascrew.shops.data;

import de.bossascrew.shops.util.Pair;
import lombok.Getter;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;

@Getter
@Setter
public class LoadedMessage implements Comparable<LoadedMessage> {

	private final String languageKey;
	private final String key;
	private final String value;
	private final String comment;
	private final Pair<String, String>[] templates;

	public LoadedMessage(String languageKey, String key, String value, Pair<String, String>... templates) {
		this(languageKey, key, value, "", templates);
	}

	public LoadedMessage(String languageKey, String key, String value, String comment, Pair<String, String>... templates) {
		this.languageKey = languageKey;
		this.key = key;
		this.value = value;
		this.comment = comment;
		this.templates = templates;
	}


	@Override
	public int compareTo(@NotNull LoadedMessage o) {
		return key.compareTo(o.key);
	}
}
