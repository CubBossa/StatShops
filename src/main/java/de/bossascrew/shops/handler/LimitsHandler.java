package de.bossascrew.shops.handler;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ListMenuElementHolder;
import de.bossascrew.shops.shop.Limit;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import net.kyori.adventure.text.Component;

import java.util.*;

@Getter
public class LimitsHandler implements
		WebAccessable<Limit>,
		ListMenuElementHolder<Limit> {

	@Getter
	private static LimitsHandler instance;

	private final Map<UUID, Limit> limitMap;
	private final Map<String, Collection<Limit>> tagMap;

	//TODO schöner strukturieren, besser durchdenken
	//TODO alle menüs aktuallisieren, wenn sich ein globales limit verändert

	public LimitsHandler() {
		instance = this;

		this.limitMap = new HashMap<>();
		this.tagMap = new HashMap<>();
	}

	public List<Limit> getLimits() {
		return new ArrayList<>(limitMap.values());
	}

	public void handleLimitDisplay(ShopEntry shopEntry, List<Component> existingLore) {

	}

	public void isLimited(Customer customer, String... tags) {
		for (String tag : tags) {
			for (Limit limit : tagMap.getOrDefault(tag, new HashSet<>())) {
				//TODO transactionmap des customers prüfen

			}
		}
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
