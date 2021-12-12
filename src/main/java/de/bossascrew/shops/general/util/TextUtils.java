package de.bossascrew.shops.general.util;

import de.bossascrew.shops.statshops.StatShops;
import lombok.experimental.UtilityClass;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.legacy.LegacyComponentSerializer;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

@UtilityClass
public class TextUtils {

	private static final LegacyComponentSerializer LEGACY_SERIALIZER = LegacyComponentSerializer.builder()
			.character('§')
			.hexColors()
			.hexCharacter('x')
			.useUnusualXRepeatedCharacterHexFormat()
			.build();

	private static final PlainTextComponentSerializer PLAIN_SERIALIZER = PlainTextComponentSerializer.builder().build();

	public static final String DURATION_FORMAT = new DurationParser(true).format(0);

	public static final String DATE_TIME_FORMAT = "dd.MM.yy, HH:mm";
	private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern(DATE_TIME_FORMAT);

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
		return toLegacy(StatShops.getInstance().getMiniMessage().parse(minimessage));
	}

	public String formatDuration(Duration duration) {
		return new DurationParser().format(duration);
	}

	public Duration parseDuration(String input) {
		return new DurationParser().parse(input);
	}

	public String formatLocalDateTime(@Nullable LocalDateTime localDateTime) {
		if (localDateTime == null) {
			return "-";
		}
		return localDateTime.format(DATE_TIME_FORMATTER);
	}

	public @Nullable LocalDateTime parseLocalDateTime(String string) {
		try {
			return LocalDateTime.parse(string, DATE_TIME_FORMATTER);
		} catch (DateTimeParseException e) {
			e.printStackTrace();
			return null;
		}
	}

}
