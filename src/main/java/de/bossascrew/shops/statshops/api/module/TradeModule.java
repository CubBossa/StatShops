package de.bossascrew.shops.statshops.api.module;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.entry.ArticleSubModule;
import de.bossascrew.shops.statshops.shop.entry.CostsSubModule;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Nullable;

public interface TradeModule extends EntryModule {

	ArticleSubModule<?> getArticle();

	void setArticle(ArticleSubModule<?> article);

	CostsSubModule<?> getCosts();

	void setCosts(CostsSubModule<?> costs);

	boolean isPurchasable();

	void setPurchasable(boolean purchasable);

	boolean isSellable();

	void setSellable(boolean sellable);

	boolean isPurchasableStacked();

	void setPurchasableStacked(boolean purchasableStacked);

	boolean isSellableStacked();

	void setSellableStacked(boolean sellableStacked);

	Component getPriceDisplay(@Nullable Customer customer, boolean buy);

	Component getPriceDisplay(@Nullable Customer customer, boolean buy, double discount);

	Transaction getLastTransaction(Customer customer);

	Price<?> getPayPrice(boolean buy);

	Price<?> getGainPrice();
}
