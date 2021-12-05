package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;

public abstract class Reward {

	public abstract boolean canPayReward(Customer customer, TradeModule<?> shopEntry);

	public abstract void payReward(Customer customer, TradeModule<?> shopEntry);
}
