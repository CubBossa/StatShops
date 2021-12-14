package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import net.kyori.adventure.text.Component;

public interface TradeModule<P, G> extends EntryModule {

	boolean isBuyable();

	boolean isSellable();

	boolean isBuyableStacked();

	boolean isSellableStacked();

	Component getPriceDisplay(boolean buy);

	Transaction getLastTransaction(Customer customer);

	Price<P> getPayPrice(boolean buy);

	Price<G> getGainPrice();
}
