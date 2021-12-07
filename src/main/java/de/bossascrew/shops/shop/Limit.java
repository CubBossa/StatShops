package de.bossascrew.shops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.google.common.collect.Lists;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.DatabaseObject;
import de.bossascrew.shops.handler.LimitsHandler;
import de.bossascrew.shops.menu.ListMenuElement;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.Duplicable;
import de.bossascrew.shops.util.Editable;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Data;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

@Data
public class Limit implements
		Taggable,
		Comparable<Limit>,
		Editable<Player>,
		ListMenuElement,
		DatabaseObject,
		Duplicable<Limit> {

	private final UUID uuid;
	private String nameFormat;
	@JsonIgnore
	private Component name;
	@JsonIgnore
	private String namePlain;
	private @Nullable String permission;
	private Duration recover;
	@JsonIgnore
	private Predicate<Customer> appliesToCustomer;
	private int transactionLimit;
	/**
	 * if set to true, all bought items with a tag that is also contained in this taggable will be summed before checking the limit
	 */
	private boolean summTagMemberLimits = false;
	private final List<String> tags;

	@JsonIgnore
	private Player editor;

	public Limit(String nameFormat, Duration recover, Predicate<Customer> appliesToCustomer, int limit, String... tags) {
		this.uuid = UUID.randomUUID();
		setNameFormat(nameFormat);
		this.recover = recover;
		this.appliesToCustomer = appliesToCustomer;
		this.transactionLimit = limit;
		this.tags = Lists.newArrayList(tags);
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = ShopPlugin.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = ComponentUtils.toPlain(name);
	}

	@Override
	public boolean addTag(String tag) {
		if (hasTag(tag)) {
			return false;
		}
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
	public int compareTo(@NotNull Limit o) {
		return Integer.compare(this.transactionLimit, o.transactionLimit);
	}

	@Override
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createLimitsItemStack(this);
	}

	@Override
	public void saveToDatabase() {
		ShopPlugin.getInstance().getDatabase().saveLimit(this);
	}

	@Override
	public Limit duplicate() {
		return LimitsHandler.getInstance().createDuplicate(this);
	}
}
