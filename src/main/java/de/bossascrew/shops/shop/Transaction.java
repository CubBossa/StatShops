package de.bossascrew.shops.shop;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.entry.ShopEntry;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Data
public class Transaction {

	private final Customer customer;
	private final ShopEntry shopEntry;
	private final LocalDateTime localDateTime;
	private final List<Discount> accountedDiscounts;
}
