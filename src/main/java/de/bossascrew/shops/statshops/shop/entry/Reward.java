package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.TradeModule;

public abstract class Reward {

	public abstract boolean canPayReward(Customer customer, TradeModule<?> shopEntry);

	public abstract void payReward(Customer customer, TradeModule<?> shopEntry);
}
