package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.data.Message;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.ComponentLike;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;

@Getter
@RequiredArgsConstructor
public class AnvilInputValidator<T> {

	private static final Predicate<String> PREDICATE_TAG = s -> true;
	private static final Predicate<String> PREDICATE_PERMISSION = s -> !s.trim().contains(" ");
	private static final Predicate<String> PREDICATE_PERCENT = s -> Pattern.matches("^-?[0-9]+([,.][0-9]{0,3})?%$", s.trim());
	private static final Predicate<String> PREDICATE_INT = s -> Pattern.matches("[0-9]+", s.trim());
	private static final Predicate<String> PREDICATE_FLOAT = s -> Pattern.matches("[0-9]+[,.][0-9]*", s.trim());
	private static final Predicate<String> PREDICATE_DURATION = s -> Pattern.matches(TextUtils.DURATION_FORMAT_REGEX, s.trim());
	private static final Predicate<String> PREDICATE_DATE_TIME = s -> TextUtils.parseLocalDateTime(s.trim()) != null;

	public static final AnvilInputValidator<String> VALIDATE_TAG = new AnvilInputValidator<>(Message.ERROR_PARSE_STRING,
			Component.text("<tag>"), PREDICATE_TAG, String::trim);
	public static final AnvilInputValidator<String> VALIDATE_PERMISSION = new AnvilInputValidator<>(Message.ERROR_PARSE_STRING,
			Component.text("<permission> (no spaces)"), PREDICATE_PERMISSION, String::trim);
	public static final AnvilInputValidator<Double> VALIDATE_PERCENT = new AnvilInputValidator<>(Message.ERROR_PARSE_PERCENT,
			Component.text("<float value>%"), PREDICATE_PERCENT, s -> Double.parseDouble(s.replace("%", "").trim()));
	public static final AnvilInputValidator<Integer> VALIDATE_INT = new AnvilInputValidator<>(Message.ERROR_PARSE_INTEGER,
			Component.text("Integer"), PREDICATE_INT, s -> Integer.parseInt(s.trim()));
	public static final AnvilInputValidator<Double> VALIDATE_FLOAT = new AnvilInputValidator<>(Message.ERROR_PARSE_DOUBLE,
			Component.text("Float"), PREDICATE_FLOAT, s -> Double.parseDouble(s.trim()));
	public static final AnvilInputValidator<Duration> VALIDATE_DURATION = new AnvilInputValidator<>(Message.ERROR_PARSE_DURATION,
			Component.text(TextUtils.DURATION_FORMAT), PREDICATE_DURATION, s -> TextUtils.parseDuration(s.trim()));
	public static final AnvilInputValidator<LocalDateTime> VALIDATE_DATE_TIME = new AnvilInputValidator<>(Message.ERROR_PARSE_DATE,
			Component.text(TextUtils.DATE_TIME_FORMAT_SHORT), PREDICATE_DATE_TIME, s -> TextUtils.parseLocalDateTime(s.trim()));


	private final Message errorMessage;
	private final ComponentLike requiredFormat;
	private final Predicate<String> inputValidator;
	private final Function<String, T> inputParser;
}
