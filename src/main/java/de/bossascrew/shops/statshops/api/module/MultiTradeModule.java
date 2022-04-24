package de.bossascrew.shops.statshops.api.module;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.entry.ArticleSubModule;
import de.bossascrew.shops.statshops.shop.entry.CostsSubModule;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface MultiTradeModule extends EntryModule {

	List<ArticleSubModule<?>> getArticles();

	void addArticle(ArticleSubModule<?> article);

	void removeArticle(ArticleSubModule<?> article);

	List<CostsSubModule<?>> getCosts();

	void addCosts(CostsSubModule<?> costs);

	void removeCosts(CostsSubModule<?> costs);

	boolean isPurchasable();

	void setPurchasable(boolean purchasable);

	boolean isPurchasableStacked();

	void setPurchasableStacked(boolean purchasableStacked);

	boolean isSellable();

	void setSellable(boolean sellable);

	boolean isSellableStacked();

	void setSellableStacked(boolean sellableStacked);

	List<Component> getPriceDisplay(@Nullable Customer customer, boolean buy);

	List<Component> getPriceDisplay(@Nullable Customer customer, boolean buy, double discount);

	Transaction getLastTransaction(Customer customer);

	List<Price<?>> getPayPrice(boolean buy);

	List<Price<?>> getGainPrice();
}
