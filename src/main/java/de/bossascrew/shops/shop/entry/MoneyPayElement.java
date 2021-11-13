package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.shop.ShopInteractionResult;

public class MoneyPayElement implements EntryElement {

	private final double costs;

	public MoneyPayElement(double costs) {
		this.costs = costs;
	}

	@Override
	public String getAmountDisplay() {
		return costs + " " + (costs == 1 ? ShopPlugin.getEconomy().currencyNameSingular() : ShopPlugin.getEconomy().currencyNamePlural());
	}

	public boolean canAct(Customer customer) {
		return ShopPlugin.getEconomy().has(customer.getPlayer(), costs);
	}

	public ShopInteractionResult act(Customer customer) {
		return ShopPlugin.getEconomy().withdrawPlayer(customer.getPlayer(), costs).transactionSuccess() ?
				ShopInteractionResult.SUCCESS :
				ShopInteractionResult.FAIL_CANT_AFFORD;
	}
}
