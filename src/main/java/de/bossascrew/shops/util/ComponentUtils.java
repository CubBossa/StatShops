package de.bossascrew.shops.util;

import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;

@UtilityClass
public class ComponentUtils {

	private static final LegacyComponentSerializer SERIALIZER = LegacyComponentSerializer.builder()
			.hexColors()
			.hexCharacter('#')
			.character('§')
			.build();

	public String toLegacy(Component component) {
		return SERIALIZER.serialize(component);
	}

}
