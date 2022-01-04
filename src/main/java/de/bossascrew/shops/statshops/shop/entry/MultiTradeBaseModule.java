package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.module.MultiTradeModule;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import net.kyori.adventure.text.Component;

import java.util.List;

public class MultiTradeBaseModule extends BaseModule implements MultiTradeModule {

	public MultiTradeBaseModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry) {
		super(provider, shopEntry);
	}

	@Override
	public List<ArticleSubModule<?>> getArticle() {
		return null;
	}

	@Override
	public void addArticle(ArticleSubModule<?> article) {

	}

	@Override
	public void removeArticle(ArticleSubModule<?> article) {

	}

	@Override
	public CostsSubModule<?> getCosts() {
		return null;
	}

	@Override
	public void setCosts(CostsSubModule<?> costs) {

	}

	@Override
	public boolean isPurchasableStacked() {
		return false;
	}

	@Override
	public void setPurchasableStacked(boolean purchasableStacked) {

	}

	@Override
	public Component getPriceDisplay() {
		return null;
	}

	@Override
	public Component getPriceDisplay(double discount) {
		return null;
	}

	@Override
	public Transaction getLastTransaction(Customer customer) {
		return null;
	}

	@Override
	public Price<?> getPayPrice() {
		return null;
	}

	@Override
	public List<Price<?>> getGainPrice() {
		return null;
	}
}
