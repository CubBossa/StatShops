package de.bossascrew.shops.shop;

import lombok.Data;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.NotNull;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UUID;

@Data
public class Discount implements Taggable, Comparable<Discount> {

	private final UUID uuid;
	private final Component name;
	private final LocalDateTime startTime;
	private final Duration duration;
	private final double percent;
	private final String permission;
	private final List<String> tags;

	/**
	 * @return The remaining time in seconds
	 */
	public long getRemaining() {
		return LocalDateTime.now().until(startTime.plus(duration), ChronoUnit.SECONDS);
	}

	@Override
	public boolean addTag(String tag) {
		return tags.add(tag);
	}

	@Override
	public boolean removeTag(String tag) {
		return tags.remove(tag);
	}

	@Override
	public boolean hasTag(String tag) {
		return tags.contains(tag);
	}

	@Override
	public int compareTo(@NotNull Discount o) {
		return Double.compare(percent, o.percent);
	}
}
