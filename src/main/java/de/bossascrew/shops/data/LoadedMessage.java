package de.bossascrew.shops.data;

import de.bossascrew.shops.util.Pair;
import lombok.Data;

@Data
public class LoadedMessage {

	private final String key;
	private final String value;
	private final Pair<String, String>[] templates;
}
