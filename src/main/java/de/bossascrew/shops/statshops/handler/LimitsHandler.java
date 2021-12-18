package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Taggable;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.Limit;
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

		subscribers = new HashMap<>();
		tagMap = new HashMap<>();
		limitMap = StatShops.getInstance().getDatabase().loadLimits();
		for (Limit limit : limitMap.values()) {
			for (String tag : limit.getTags()) {
				Collection<Limit> limits = tagMap.getOrDefault(tag, new ArrayList<>());
				limits.add(limit);
				tagMap.put(tag, limits);
			}
		}
	}

	public List<Limit> getLimits() {
		return new ArrayList<>(limitMap.values());
	}

	public void handleLimitTagAdded(Limit limit, String tag) {
		Collection<Limit> limits = tagMap.getOrDefault(tag, new ArrayList<>());
		limits.add(limit);
		tagMap.put(tag, limits);
		updateAllSubscribers(limit);
	}

	public void handleLimitTagRemoved(Limit limit, String tag) {
		tagMap.get(tag).remove(limit);
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
		Pair<Limit, Limit> pair = getMinimalLimitsWithMatchingTags(shopEntry, shopEntry.getShop());
		ItemStackUtils.addLoreLimits(existingLore, pair.getLeft(), pair.getRight(), 3);
	}

	public void isLimited(Customer customer, String... tags) {
		//TODO
	}

	public Pair<Limit, Limit> getMinimalLimitsWithMatchingTags(Taggable... taggables) {
		List<Limit> limits = getLimitsWithMatchingTags(taggables);
		Limit smallLocal = null;
		Limit smallGlobal = null;
		for (Limit limit : limits) {
			if (limit.isGlobal()) {
				if (smallGlobal == null || limit.getTransactionLimit() < smallGlobal.getTransactionLimit()) {
					smallGlobal = limit;
				}
			} else {
				if (smallLocal == null || limit.getTransactionLimit() < smallLocal.getTransactionLimit()) {
					smallLocal = limit;
				}
			}
		}
		return new Pair<>(smallGlobal, smallGlobal);
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
		Limit limit = StatShops.getInstance().getDatabase().createLimit(input);
		limitMap.put(limit.getUuid(), limit);
		return limit;
	}

	@Override
	public Limit createDuplicate(Limit element) {
		Limit limit = createNew(element.getTransactionLimit() + "");
		limit.setRecover(element.getRecover());
		limit.setAppliesToCustomer(element.getAppliesToCustomer());
		limit.setSummTagMemberLimits(element.isSummTagMemberLimits());
		StatShops.getInstance().getDatabase().saveLimit(limit);
		return limit;
	}

	@Override
	public boolean delete(Limit limit) {
		//TODO alle shops aktuallisieren, die dem limit entsprechen.

		StatShops.getInstance().getDatabase().deleteLimit(limit);
		limitMap.remove(limit.getUuid());
		return false;
	}
}
