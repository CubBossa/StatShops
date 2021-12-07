package de.bossascrew.shops.handler;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.menu.ShopMenu;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Limit;
import de.bossascrew.shops.shop.Taggable;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.Pair;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import net.kyori.adventure.text.Component;

import java.util.*;
import java.util.stream.Collectors;

@Getter
public class LimitsHandler implements
		WebAccessable<Limit>,
		ListManagementMenuElementHolder<Limit> {

	@Getter
	private static LimitsHandler instance;

	private final Map<UUID, Limit> limitMap;
	private final Map<String, Collection<Limit>> tagMap;

	private final Map<Limit, List<Pair<ShopMenu, ShopEntry>>> subscribers;

	public LimitsHandler() {
		instance = this;

		this.limitMap = new HashMap<>();
		this.tagMap = new HashMap<>();
		this.subscribers = new HashMap<>();
	}

	public List<Limit> getLimits() {
		return new ArrayList<>(limitMap.values());
	}

	public void handleLimitTagsUpdate(Limit limit) {
		updateAllSubscribers(limit);
	}

	public void subscribeToDisplayUpdates(ShopMenu menu, ShopEntry shopEntry) {
		List<Limit> limits = getLimitsWithMatchingTags(shopEntry, shopEntry.getShop());
		for (Limit limit : limits) {
			List<Pair<ShopMenu, ShopEntry>> innerSubscribers = subscribers.getOrDefault(limit, new ArrayList<>());
			innerSubscribers.add(new Pair<>(menu, shopEntry));
			subscribers.put(limit, innerSubscribers);
		}
	}

	public void unsubscribeToDisplayUpdates(ShopMenu menu) {
		subscribers.replaceAll((k, v) -> v.stream().filter(p -> !p.getLeft().equals(menu)).collect(Collectors.toList()));
	}

	public void updateAllSubscribers(Limit limit) {
		for (Pair<ShopMenu, ShopEntry> pair : subscribers.getOrDefault(limit, new ArrayList<>())) {
			pair.getLeft().updateEntry(pair.getRight());
		}
	}

	public void addLimitsLore(ShopEntry shopEntry, List<Component> existingLore) {
		//TODO
	}

	public void isLimited(Customer customer, String... tags) {
		//TODO
	}

	public List<Limit> getLimitsWithMatchingTags(Taggable... taggables) {
		List<Limit> limits = new ArrayList<>();
		for (Taggable taggable : taggables) {
			for (String tag : taggable.getTags()) {
				if (tagMap.containsKey(tag)) {
					limits.addAll(tagMap.get(tag));
				}
			}
		}
		return limits;
	}

	@Override
	public List<Limit> getWebData() {
		return getLimits();
	}

	@Override
	public void storeWebData(List<Limit> values) {
		//TODO
	}

	@Override
	public List<Limit> getValues() {
		return getLimits();
	}

	@Override
	public Limit createNew(String input) {
		Limit limit = ShopPlugin.getInstance().getDatabase().createLimit(input);
		limitMap.put(limit.getUuid(), limit);
		return limit;
	}

	@Override
	public Limit createDuplicate(Limit element) {
		Limit limit = createNew(element.getTransactionLimit() + "");
		limit.setRecover(element.getRecover());
		limit.setAppliesToCustomer(element.getAppliesToCustomer());
		limit.setSummTagMemberLimits(element.isSummTagMemberLimits());
		ShopPlugin.getInstance().getDatabase().saveLimit(limit);
		return limit;
	}

	@Override
	public boolean delete(Limit limit) {
		//TODO alle shops aktuallisieren, die dem limit entsprechen.

		ShopPlugin.getInstance().getDatabase().deleteLimit(limit);
		limitMap.remove(limit.getUuid());
		return false;
	}
}
