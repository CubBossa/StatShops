package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.menu.ShopMenu;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Taggable;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.util.Pair;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import net.kyori.adventure.text.Component;

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

	private final Map<Discount, List<Pair<ShopMenu, ShopEntry>>> subscribers;

	public DiscountHandler() {
		instance = this;

		discountMap = ShopPlugin.getInstance().getDatabase().loadDiscounts();
		tagMap = new HashMap<>();
		for (Discount discount : discountMap.values()) {
			for (String tag : discount.getTags()) {
				List<Discount> discounts = tagMap.getOrDefault(tag, new ArrayList<>());
				discounts.add(discount);
				tagMap.put(tag, discounts);
			}
		}
		subscribers = new HashMap<>();
	}

	public List<Discount> getDiscounts() {
		return new ArrayList<>(discountMap.values());
	}

	public Discount createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags) {
		Discount discount = ShopPlugin.getInstance().getDatabase().createDiscount(nameFormat, start, duration, percent, tags);
		discountMap.put(discount.getUuid(), discount);
		for (String tag : tags) {
			List<Discount> discounts = tagMap.getOrDefault(tag, new ArrayList<>());
			discounts.add(discount);
			tagMap.put(tag, discounts);
		}
		return discount;
	}

	public boolean deleteDiscount(Discount discount) {
		ShopPlugin.getInstance().getDatabase().deleteDiscount(discount);
		updateAllSubscribers(discount);
		return discountMap.remove(discount.getUuid()) != null;
	}

	private void handleDiscountStart(Discount discount) {
		updateAllSubscribers(discount);
	}

	private void handleDiscountExpire(Discount discount) {
		updateAllSubscribers(discount);
	}

	private void handleDiscountTagsUpdate(Discount discount) {
		updateAllSubscribers(discount);
	}

	public void subscribeToDisplayUpdates(ShopMenu menu, ShopEntry shopEntry) {
		List<Discount> discounts = getDiscountsWithMatchingTags(shopEntry, shopEntry.getShop());
		for (Discount discount : discounts) {
			List<Pair<ShopMenu, ShopEntry>> innerSubscribers = subscribers.getOrDefault(discount, new ArrayList<>());
			innerSubscribers.add(new Pair<>(menu, shopEntry));
			subscribers.put(discount, innerSubscribers);
		}
	}

	public void unsubscribeToDisplayUpdates(ShopMenu menu) {
		for (Map.Entry<Discount, List<Pair<ShopMenu, ShopEntry>>> pairs : subscribers.entrySet()) {
			subscribers.put(pairs.getKey(), pairs.getValue().stream().filter(p -> !p.getLeft().equals(menu)).collect(Collectors.toList()));
		}
	}

	private void updateAllSubscribers(Discount discount) {
		for (Pair<ShopMenu, ShopEntry> pair : subscribers.getOrDefault(discount, new ArrayList<>())) {
			pair.getLeft().updateEntry(pair.getRight());
		}
	}

	public void addDiscountsLore(ShopEntry shopEntry, List<Component> lore) {
		List<Discount> discounts = getDiscountsWithMatchingTags(shopEntry, shopEntry.getShop());
		ItemStackUtils.addLoreDiscount(lore, discounts);
	}

	public double combineDiscounts(Taggable... taggables) {
		double discountValue = 0;
		List<Discount> discounts = getDiscountsWithMatchingTags(taggables);
		for (Discount discount : discounts) {
			discountValue += discount.getPercent();
		}
		return discountValue;
	}

	public List<Discount> getDiscountsWithMatchingTags(Taggable... taggables) {
		List<Discount> discounts = new ArrayList<>();
		for (Taggable taggable : taggables) {
			for (String tag : taggable.getTags()) {
				if (tagMap.containsKey(tag)) {
					discounts.addAll(tagMap.get(tag));
				}
			}
		}
		return discounts;
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
		return createDiscount(input, LocalDateTime.now(), Duration.of(10, ChronoUnit.DAYS), 10);
	}

	@Override
	public Discount createDuplicate(Discount element) {
		Discount discount = createDiscount(element.getNameFormat(), element.getStartTime(), element.getDuration(), element.getPercent());
		for (String tag : element.getTags()) {
			discount.addTag(tag);
		}
		ShopPlugin.getInstance().getDatabase().saveDiscount(discount);
		return discount;
	}

	@Override
	public boolean delete(Discount element) {
		return deleteDiscount(element);
	}
}
