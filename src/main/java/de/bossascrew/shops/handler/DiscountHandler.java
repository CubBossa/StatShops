package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ShopMenuView;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Taggable;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.Inventory;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

public class DiscountHandler {

	@Getter
	private static DiscountHandler instance;

	private final Map<UUID, Discount> discountMap;
	private final Map<String, List<Discount>> tagMap;

	private final Hashtable<Discount, Map<ShopMenuView, ShopEntry>> subscribers;

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
		subscribers = new Hashtable<>();
	}

	public List<Discount> getDiscounts() {
		return new ArrayList<>(discountMap.values());
	}

	public void createDiscount(String nameFormat, LocalDateTime start, Duration duration, double percent, String... tags) {
		Discount discount = ShopPlugin.getInstance().getDatabase().createDiscount(nameFormat, start, duration, percent, tags);
		discountMap.put(discount.getUuid(), discount);
		for (String tag : tags) {
			List<Discount> discounts = tagMap.getOrDefault(tag, new ArrayList<>());
			discounts.add(discount);
			tagMap.put(tag, discounts);
		}
		handleDiscountStart(discount);
	}

	public void handleDiscountStart(Discount discount) {
		for (Map.Entry<ShopMenuView, ShopEntry> subscriber : subscribers.getOrDefault(discount, new HashMap<>()).entrySet()) {
			//Update all subscribed shop menus that are currently open (so the player sees the new price without discount)
			subscriber.getKey().update(subscriber.getValue());
		}
	}

	public void handleDiscountExpire(Discount discount) {
		for (Map.Entry<ShopMenuView, ShopEntry> subscriber : subscribers.getOrDefault(discount, new HashMap<>()).entrySet()) {
			//Update all subscribed shop menus that are currently open (so the player sees the new price without discount)
			subscriber.getKey().update(subscriber.getValue());
		}
	}

	public void handleShopClose(Inventory inventory) {
		//TODO aus allen subscribern rausnehmen
	}

	public void subscribeToDisplayUpdates(ShopMenuView view, ShopEntry shopEntry) {
		List<Discount> discounts = getDiscountsWithMatchingTags(shopEntry, shopEntry.getShop());
		for (Discount discount : discounts) {
			//TODO subscriben
		}
	}

	public void addDiscountsLore(ShopMenuView view, ShopEntry shopEntry, List<Component> lore) {
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
				discounts.addAll(tagMap.get(tag));
			}
		}
		return discounts;
	}
}
