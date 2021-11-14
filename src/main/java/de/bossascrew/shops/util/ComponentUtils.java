package de.bossascrew.shops.util;

import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;

@UtilityClass
public class ComponentUtils {

	private static final LegacyComponentSerializer LEGACY_SERIALIZER = LegacyComponentSerializer.builder()
			.hexColors()
			.hexCharacter('#')
			.character('§')
			.build();

	private static final PlainTextComponentSerializer PLAIN_SERIALIZER = PlainTextComponentSerializer.builder().build();

	public Component fromLegacy(String legacy) {
		return LEGACY_SERIALIZER.deserialize(legacy);
	}

	public String toLegacy(Component component) {
		return LEGACY_SERIALIZER.serialize(component);
	}

	public String toPlain(Component component) {
		return PLAIN_SERIALIZER.serialize(component);
	}

}
