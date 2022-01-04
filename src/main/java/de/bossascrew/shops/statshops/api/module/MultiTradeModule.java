package de.bossascrew.shops.statshops.api.module;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.entry.ArticleSubModule;
import de.bossascrew.shops.statshops.shop.entry.CostsSubModule;
import net.kyori.adventure.text.Component;

import java.util.List;

public interface MultiTradeModule {

	List<ArticleSubModule<?>> getArticle();

	void addArticle(ArticleSubModule<?> article);

	void removeArticle(ArticleSubModule<?> article);

	CostsSubModule<?> getCosts();

	void setCosts(CostsSubModule<?> costs);

	boolean isPurchasableStacked();

	void setPurchasableStacked(boolean purchasableStacked);

	Component getPriceDisplay();

	Component getPriceDisplay(double discount);

	Transaction getLastTransaction(Customer customer);

	Price<?> getPayPrice();

	List<Price<?>> getGainPrice();
}
