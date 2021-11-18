package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.shop.ShopInteractionResult;

public class MoneyReceiveElement implements EntryElement {

	private final double costs;

	public MoneyReceiveElement(double costs) {
		this.costs = costs;
	}

	@Override
	public String getAmountDisplay() {
		return costs + " " + (costs == 1 ? ShopPlugin.getEconomy().currencyNameSingular() : ShopPlugin.getEconomy().currencyNamePlural());
	}

	public boolean canAct(Customer customer) {
		return true;
	}

	public ShopInteractionResult act(Customer customer) {
		return ShopPlugin.getEconomy().depositPlayer(customer.getPlayer(), costs).transactionSuccess() ?
				ShopInteractionResult.SUCCESS :
				ShopInteractionResult.FAIL_VAULT_UNKNOWN;
	}

	@Override
	public EntryElement duplicate() {
		return new MoneyPayElement(costs);
	}
}
