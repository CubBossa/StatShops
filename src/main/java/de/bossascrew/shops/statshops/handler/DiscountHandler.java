package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.api.Taggable;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Collectors;

public class DiscountHandler implements
		WebAccessable<Discount>,
		ListManagementMenuElementHolder<Discount> {

	@Getter
	private static DiscountHandler instance;

	private final Map<UUID, Discount> discountMap;
	private final Map<String, List<Discount>> tagMap;

	private final Map<String, List<Pair<ShopMenu, ShopEntry>>> subscribers;

	public DiscountHandler() {
		instance = this;

		subscribers = new HashMap<>();
		tagMap = new HashMap<>();
		discountMap = StatShops.getInstance().getDatabase().loadDiscounts();
	}

	public List<Discount> getDiscounts() {
		return new ArrayList<>(discountMap.values());
	}

	public void addDiscount(Discount discount) {
		discountMap.put(discount.getUuid(), discount);
		for (String tag : discount.getTags()) {
			List<Discount> discounts = tagMap.getOrDefault(tag, new ArrayList<>());
			if (!discounts.contains(discount)) {
				discounts.add(discount);
				tagMap.put(tag, discounts);
			}
		}
		//Refresh all open inventories that are
		if (discount.isCurrentlyActive()) {
			handleDiscountStart(discount);
		} else {
			LocalDateTime nextStart = discount.getNextStart();
			if (nextStart != null) {
				//If nextStart is actually null (which it should not be) we have to create start a scheduler later on

				Bukkit.getScheduler().runTaskLaterAsynchronously(StatShops.getInstance(), () -> {
					handleDiscountStart(discount);
				}, LocalDateTime.now().until(nextStart, ChronoUnit.MILLIS) / 50);
			}
		}
		StatShops.getInstance().getDatabase().saveDiscount(discount);
	}

	public Discount createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags) {
		TreeSet<LocalDateTime> startTimes = new TreeSet<>();
		startTimes.add(start);
		return createDiscount(nameFormat, startTimes, duration, percent, tags);
	}

	public Discount createDiscount(String nameFormat, SortedSet<LocalDateTime> start, Duration duration, double percent, String... tags) {
		Discount discount = new Discount(UUID.randomUUID(), nameFormat, start, duration, percent, null, tags);

		addDiscount(discount);
		return discount;
	}

	public boolean deleteDiscount(Discount discount) {
		for (String tag : new ArrayList<>(discount.getTags())) {
			discount.removeTag(tag);
		}
		StatShops.getInstance().getDatabase().deleteDiscount(discount);
		return discountMap.remove(discount.getUuid()) != null;
	}

	public void handleDiscountStart(Discount discount) {
		updateAllSubscribers(discount);
		Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> {
			handleDiscountExpire(discount);
		}, discount.getDurationSeconds() * 20 + 1);
	}

	public void handleDiscountExpire(Discount discount) {
		updateAllSubscribers(discount);
	}

	public void handleDiscountTagRemoved(Discount discount, String tag) {
		tagMap.get(tag).remove(discount);
		updateAllSubscribers(discount);
	}

	public void handleDiscountTagAdded(Discount discount, String tag) {
		List<Discount> discounts = tagMap.getOrDefault(tag, new ArrayList<>());
		discounts.add(discount);
		tagMap.put(tag, discounts);
		updateAllSubscribers(discount);
	}

	public void subscribeToDisplayUpdates(ShopMenu menu, ShopEntry shopEntry) {
		List<String> tags = shopEntry.getTags();
		tags.addAll(shopEntry.getShop().getTags());
		for (String tag : tags) {
			List<Pair<ShopMenu, ShopEntry>> innerSubscribers = subscribers.getOrDefault(tag, new ArrayList<>());
			innerSubscribers.add(new Pair<>(menu, shopEntry));
			subscribers.put(tag, innerSubscribers);
		}
	}

	public void unsubscribeToDisplayUpdates(ShopMenu menu) {
		subscribers.replaceAll((k, v) -> v.stream().filter(p -> !p.getLeft().equals(menu)).collect(Collectors.toList()));
	}

	private void updateAllSubscribers(Discount discount) {
		List<Pair<ShopMenu, ShopEntry>> toIterate = new ArrayList<>();
		for (String tag : discount.getTags()) {
			toIterate.addAll(subscribers.getOrDefault(tag, new ArrayList<>()));
		}
		for (Pair<ShopMenu, ShopEntry> pair : toIterate) {
			pair.getLeft().updateEntry(pair.getRight());
		}
	}

	/**
	 * @param discounts All provided discounts will be divided by 100 and summed to 1. 80% -> 1.8
	 * @param invert    If the result is supposed to be inverted to 1. 1.8 -> 0.2
	 * @return the sum of all discount percentages
	 */
	public double combineDiscounts(List<Discount> discounts, boolean invert) {
		double discountValue = 1;
		for (Discount discount : discounts) {
			discountValue += discount.getPercent() / 100;
		}
		return invert ? (discountValue - 1) * -1 + 1 : discountValue;
	}

	public double combineDiscountsWithMatchingTags(@Nullable Player player, boolean invert, Taggable... taggables) {
		List<Discount> discounts = getDiscountsWithMatchingTags(player, taggables);
		return combineDiscounts(discounts, invert);
	}

	public List<Discount> getDiscountsWithMatchingTags(@Nullable Player player, Taggable... taggables) {
		Collection<Discount> discounts = new LinkedHashSet<>();
		for (Taggable taggable : taggables) {
			for (String tag : taggable.getTags()) {
				if (tagMap.containsKey(tag)) {
					discounts.addAll(tagMap.get(tag).stream().filter(discount -> player == null || discount.getPermission() == null || player.hasPermission(discount.getPermission()))
							.collect(Collectors.toList()));
				}
			}
		}
		discounts = discounts.stream().filter(Discount::isCurrentlyActive).collect(Collectors.toList());
		return new ArrayList<>(discounts);
	}

	@Override
	public List<Discount> getWebData() {
		return getDiscounts();
	}

	@Override
	public void storeWebData(List<Discount> values) {
		//TODO
	}

	@Override
	public List<Discount> getValues() {
		return getDiscounts();
	}

	@Override
	public Discount createNew(String input) {
		return createDiscount(input, LocalDateTime.now(), Duration.of(1, ChronoUnit.DAYS), 10);
	}

	@Override
	public Discount createDuplicate(Discount element) {
		Discount discount = createDiscount(element.getNameFormat(), element.getStartTimes(), element.getDuration(), element.getPercent());
		for (String tag : element.getTags()) {
			discount.addTag(tag);
		}
		StatShops.getInstance().getDatabase().saveDiscount(discount);
		return discount;
	}

	@Override
	public boolean delete(Discount element) {
		return deleteDiscount(element);
	}
}
