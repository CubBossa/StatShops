package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.currency.SimplePrice;
import net.kyori.adventure.text.Component;

public interface TradeModule<P, G> extends EntryModule {

	boolean isBuyable();

	void setBuyable(boolean buyable);

	boolean isSellable();

	void setSellable(boolean sellable);

	boolean isBuyableStacked();

	void setBuyableStacked(boolean buyableStacked);

	boolean isSellableStacked();

	void setSellableStacked(boolean sellableStacked);

	Component getPriceDisplay(boolean buy);

	Component getPriceDisplay(boolean buy, double discount);

	Transaction getLastTransaction(Customer customer);

	Price<P> getPayPrice(boolean buy);

	Price<G> getGainPrice();
}
