package de.bossascrew.shops.statshops.data;

import com.fasterxml.jackson.annotation.JsonProperty;
import de.bossascrew.shops.general.util.Pair;
import lombok.Getter;
import lombok.Setter;
import org.jetbrains.annotations.NotNull;

@Getter
@Setter
public class LoadedMessage implements Comparable<LoadedMessage> {

	@JsonProperty("lang_key")
	private final String languageKey;
	@JsonProperty("message_key")
	private final String key;
	@JsonProperty("message_value")
	private final String value;
	@JsonProperty("comment")
	private final String comment;
	@JsonProperty("placeholders")
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
