package de.bossascrew.shops.util;

import de.bossascrew.shops.ShopPlugin;
import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@UtilityClass
public class ComponentUtils {

	private static final LegacyComponentSerializer LEGACY_SERIALIZER = LegacyComponentSerializer.builder()
			.character('§')
			.hexColors()
			.hexCharacter('x')
			.useUnusualXRepeatedCharacterHexFormat()
			.build();

	private static final PlainTextComponentSerializer PLAIN_SERIALIZER = PlainTextComponentSerializer.builder().build();

	private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yy, hh:mm");

	public Component fromLegacy(String legacy) {
		return LEGACY_SERIALIZER.deserialize(legacy);
	}

	public String toLegacy(Component component) {
		return LEGACY_SERIALIZER.serialize(component);
	}

	public String toPlain(Component component) {
		return PLAIN_SERIALIZER.serialize(component);
	}

	public String toLegacyFromMiniMessage(String minimessage) {
		return toLegacy(ShopPlugin.getInstance().getMiniMessage().parse(minimessage));
	}

	public String formatDuration(Duration duration) {
		return new DurationParser().parse(duration);
	}

	public String formatLocalDateTime(LocalDateTime localDateTime) {
		return localDateTime.format(DATE_TIME_FORMATTER);
	}

}
