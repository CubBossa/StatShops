package de.bossascrew.shops.statshops.shop;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Lists;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.data.NamedObject;
import de.bossascrew.shops.statshops.api.Taggable;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.general.util.Editable;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.data.DatabaseObject;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import lombok.Data;
import net.kyori.adventure.text.Component;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Predicate;

@Data
public class Limit implements
		Taggable,
		Comparable<Limit>,
		Editable<Player>,
		DatabaseObject,
		Duplicable<Limit>,
		NamedObject {

	private final UUID uuid;
	private String nameFormat;
	@JsonIgnore
	private Component name;
	@JsonIgnore
	private String namePlain;
	private @Nullable String permission;
	private Duration recover;
	private boolean global = false;
	@JsonIgnore
	private Predicate<Customer> appliesToCustomer;
	private int transactionLimit;
	private final List<String> tags;

	@JsonIgnore
	private Player editor;

	public Limit(String nameFormat, Duration recover, Predicate<Customer> appliesToCustomer, int limit, String... tags) {
		this(UUID.randomUUID(), nameFormat, recover, appliesToCustomer, limit, tags);
	}

	public Limit(UUID uuid, String nameFormat, Duration recover, Predicate<Customer> appliesToCustomer, int limit, String... tags) {
		this.uuid = uuid;
		setNameFormat(nameFormat);
		this.recover = recover;
		this.appliesToCustomer = appliesToCustomer;
		this.transactionLimit = limit;
		this.tags = Lists.newArrayList(tags);
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().deserialize(nameFormat);
		this.namePlain = TextUtils.toPlain(name);
	}


	public List<String> getTags(boolean generated) {
		return new ArrayList<>(tags);
	}

	@Override
	public boolean addTag(String tag) {
		if (hasTag(tag)) {
			return false;
		}
		tags.add(tag);
		LimitsHandler.getInstance().handleLimitTagAdded(this, tag);
		return true;
	}

	@Override
	public boolean removeTag(String tag) {
		tags.remove(tag);
		LimitsHandler.getInstance().handleLimitTagRemoved(this,   tag);
		return true;
	}

	@Override
	public boolean hasTag(String tag) {
		return tags.contains(tag);
	}

	@Override
	public int compareTo(@NotNull Limit o) {
		return Integer.compare(this.transactionLimit, o.transactionLimit);
	}

	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createLimitsItemStack(this);
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveLimit(this);
	}

	@Override
	public Limit duplicate() {
		return LimitsHandler.getInstance().createDuplicate(this);
	}
}
