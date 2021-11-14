package de.bossascrew.shops.shop;

import de.bossascrew.shops.ShopPlugin;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@RequiredArgsConstructor
public class Discount implements Taggable, Comparable<Discount> {

	private final UUID uuid;
	private String nameFormat;
	private @Nullable Component name;
	private LocalDateTime startTime;
	private Duration duration;
	private double percent;
	private String permission;
	private final List<String> tags;

	/**
	 * @return The remaining time in seconds
	 */
	public long getRemaining() {
		return LocalDateTime.now().until(startTime.plus(duration), ChronoUnit.SECONDS);
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = ShopPlugin.getInstance().getMiniMessage().parse(nameFormat);
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
