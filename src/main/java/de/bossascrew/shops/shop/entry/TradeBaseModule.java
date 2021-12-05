package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.LogEntry;
import de.bossascrew.shops.shop.Currency;
import de.bossascrew.shops.shop.ShopInteractionResult;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

@Getter
@Setter
public class TradeBaseModule<T> implements TradeModule<T> {

	private Currency<T> currency;
	private double priceAmount;
	private T priceObject;

	public TradeBaseModule(Currency<T> currency, double priceAmount, T priceObject) {
		this.currency = currency;
		this.priceAmount = priceAmount;
		this.priceObject = priceObject;
	}

	@Override
	public void setPrice(double amount, T object) {
		this.priceAmount = amount;
		this.priceObject = object;
	}

	@Override
	public void giveArticle(Customer customer) {

	}

	@Override
	public ItemStack getDisplayItem() {
		return null; //TODO
	}

	@Override
	public void loadData() {

	}

	@Override
	public void saveData() {

	}

	@Override
	public @Nullable LogEntry createLogEntry() {
		return new LogEntry();
	}

	@Override
	public ShopInteractionResult perform(Customer customer) {
		return null; //TODO
	}
}
