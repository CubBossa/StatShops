package de.bossascrew.shops.statshops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Lists;
import de.bossascrew.shops.statshops.api.data.NamedObject;
import de.bossascrew.shops.statshops.api.Taggable;
import de.bossascrew.shops.general.menu.ListMenuElement;
import de.bossascrew.shops.general.util.*;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.data.DatabaseObject;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;

@Getter
@Setter
public class Discount implements
		Taggable,
		Comparable<Discount>,
		Editable<Player>,
		ListMenuElement,
		DatabaseObject,
		Duplicable<Discount>,
		NamedObject {

	private final UUID uuid;
	private String nameFormat;
	@JsonIgnore
	private Component name;
	@JsonIgnore
	private String namePlain;
	@JsonIgnore
	private final SortedSet<LocalDateTime> startTimes;
	@JsonIgnore
	private Duration duration;
	private double percent;
	private String permission;
	private final List<String> tags;

	@JsonIgnore
	private @Nullable Player editor = null;

	public Discount(UUID uuid, String nameFormat, LocalDateTime startTime, Duration duration, double percent, String permission, String... tags) {
		this(uuid, nameFormat, new TreeSet<>(), duration, percent, permission, tags);
		addStartTime(startTime);
	}

	public Discount(UUID uuid, String nameFormat, SortedSet<LocalDateTime> startTimes, Duration duration, double percent, String permission, String... tags) {
		this.uuid = uuid;
		setNameFormat(nameFormat);
		this.startTimes = startTimes;
		this.duration = duration;
		this.percent = percent;
		this.permission = permission;
		this.tags = Lists.newArrayList(tags);
	}

	/**
	 * Checks if new duration has intersections with given start dates.
	 */
	public void setDuration(Duration duration) {
		Iterator<LocalDateTime> iterator = startTimes.iterator();
		if (iterator.hasNext()) {
			LocalDateTime a;
			LocalDateTime b = iterator.next();
			while (iterator.hasNext()) {
				a = b;
				b = iterator.next();

				if (a.until(b, ChronoUnit.SECONDS) > duration.getSeconds()) {
					StatShops.getInstance().log(LoggingPolicy.ERROR, "Cannot set duration, intersecting start dates: " + a + ", " + b);
					return;
				}
			}
		}
		this.duration = duration;
	}

	public boolean removeStartTime(LocalDateTime startTime) {
		if (startTimes.size() <= 1) {
			return false;
		}
		return startTimes.remove(startTime);
	}

	public boolean addStartTime(LocalDateTime startTime) {
		Iterator<LocalDateTime> iterator = startTimes.iterator();
		int duration = (int) getDurationSeconds();
		while (iterator.hasNext()) {
			LocalDateTime actual = iterator.next();
			if (Math.abs(startTime.until(actual, ChronoUnit.SECONDS)) < duration) {
				return false;
			}
		}
		return startTimes.add(startTime);
	}

	/**
	 * @return The next start time to pass
	 */
	public @Nullable LocalDateTime getNextStart() {
		LocalDateTime now = LocalDateTime.now();
		for (LocalDateTime startTime : startTimes) {
			if (startTime.isBefore(now)) {
				continue;
			}
			return startTime;
		}
		return null;
	}

	/**
	 * @return The last start time that has passed
	 */
	public LocalDateTime getLastStart() {
		LocalDateTime now = LocalDateTime.now();
		LocalDateTime searched = null;
		for (LocalDateTime startTime : startTimes) {
			if (startTime.isAfter(now)) {
				// list is sorted by date
				return searched;
			}
			// only set value because we may find a later start date.
			searched = startTime;
		}
		return searched;
	}

	/**
	 * @return The remaining time in seconds
	 */
	public long getRemaining() {
		LocalDateTime lastStart = getLastStart();
		if (lastStart == null) {
			return 0;
		}
		return LocalDateTime.now().until(lastStart.plus(duration), ChronoUnit.SECONDS);
	}

	public boolean isCurrentlyActive() {
		long remaining = getRemaining();
		return remaining > 0 && remaining < getDurationSeconds();
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().deserialize(nameFormat);
		this.namePlain = TextUtils.toPlain(name);
	}

	@JsonIgnore
	public Component getFormattedPercent() {
		if (percent > 0) {
			return Message.SHOP_ITEM_LORE_DISCOUNT_POSITIVE.getTranslation(TagResolver.resolver("discount", Tag.inserting(Component.text("" + percent))));
		}
		return Message.SHOP_ITEM_LORE_DISCOUNT_NEGATIVE.getTranslation(TagResolver.resolver("discount", Tag.inserting(Component.text("" + percent))));
	}

	@Override
	public List<String> getTags(boolean generated) {
		return new ArrayList<>(tags);
	}

	@Override
	public boolean addTag(String tag) {
		if (hasTag(tag)) {
			return false;
		}
		tags.add(tag);
		DiscountHandler.getInstance().handleDiscountTagAdded(this, tag);
		return true;
	}

	@Override
	public boolean removeTag(String tag) {
		boolean ret = tags.remove(tag);
		DiscountHandler.getInstance().handleDiscountTagRemoved(this, tag);
		return ret;
	}

	@Override
	public boolean hasTag(String tag) {
		return tags.contains(tag);
	}

	@Override
	public int compareTo(@NotNull Discount o) {
		return Double.compare(percent, o.percent);
	}

	@Override
	@JsonIgnore
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createDiscountItemStack(this);
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveDiscount(this);
	}

	@Override
	public Discount duplicate() {
		return DiscountHandler.getInstance().createDuplicate(this);
	}

	public long getDurationSeconds(){
		return duration.getSeconds();
	}
}
