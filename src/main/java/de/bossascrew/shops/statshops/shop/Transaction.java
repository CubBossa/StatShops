package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class Transaction {

	private final Customer customer;
	private final ShopEntry shopEntry;
	private final Currency<?> currency;
	private final LocalDateTime localDateTime;
	private final List<Discount> accountedDiscounts;
}
