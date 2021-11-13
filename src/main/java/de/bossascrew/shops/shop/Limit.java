package de.bossascrew.shops.shop;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.google.common.collect.Lists;
import de.bossascrew.shops.Customer;
import lombok.Data;
import org.jetbrains.annotations.NotNull;

import java.time.Duration;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;

@Data
public class Limit implements Taggable {

	private final UUID uuid;
	private Duration recover;
	private Predicate<Customer> appliesToCustomer;
	private int transactionLimit;
	/**
	 * if set to true, all bought items with a tag that is also contained in this taggable will be summed before checking the limit
	 */
	private boolean summTagMemberLimits = false;
	private final List<String> tags;
	private final Cache<@NotNull UUID, @NotNull Transaction> transactionCache;

	public Limit(Duration recover, Predicate<Customer> appliesToCustomer, int limit, String... tags) {
		this.uuid = UUID.randomUUID();
		this.recover = recover;
		this.appliesToCustomer = appliesToCustomer;
		this.transactionLimit = limit;
		this.tags = Lists.newArrayList(tags);
		this.transactionCache = Caffeine.newBuilder()
				.expireAfterWrite(recover.getSeconds(), TimeUnit.SECONDS) //TODO vllt in TransactionsHandler
				.build();
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
}
