package de.bossascrew.shops.shop;

import com.google.common.collect.Lists;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.menu.ListMenuElement;
import de.bossascrew.shops.util.Duplicable;
import de.bossascrew.shops.util.Editable;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
public class Discount implements
		Taggable,
		Comparable<Discount>,
		Editable<Player>,
		ListMenuElement,
		DatabaseObject,
		Duplicable<Discount> {

	private final UUID uuid;
	private String nameFormat;
	private Component name;
	private LocalDateTime startTime;
	private Duration duration;
	private double percent;
	private String permission;
	private final List<String> tags;

	private @Nullable Player editor = null;

	public Discount(UUID uuid, String nameFormat, LocalDateTime startTime, Duration duration, double percent, String permission, String... tags) {
		this.uuid = uuid;
		setNameFormat(nameFormat);
		this.startTime = startTime;
		this.duration = duration;
		this.percent = percent;
		this.permission = permission;
		this.tags = Lists.newArrayList(tags);
	}

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

	public Component getFormattedPercent(boolean negativeGreen) {
		if (percent > 0) {
			return Component.text("-" + percent, negativeGreen ? NamedTextColor.GREEN : NamedTextColor.RED);
		}
		return Component.text("+" + percent, negativeGreen ? NamedTextColor.RED : NamedTextColor.GREEN);
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

	@Override
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createDiscountItemStack(this);
	}

	@Override
	public void saveToDatabase() {
		ShopPlugin.getInstance().getDatabase().saveDiscount(this);
	}

	@Override
	public Discount duplicate() {
		return DiscountHandler.getInstance().createDuplicate(this);
	}
}
