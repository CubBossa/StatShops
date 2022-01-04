package de.bossascrew.shops.statshops.shop.entry;

import com.google.common.collect.Lists;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Getter
@Setter
public class TradeBaseModule extends BaseModule implements TradeModule {

	private ArticleSubModule<?> article;
	private CostsSubModule<?> costs;
	private final Map<UUID, Transaction> lastTransactions;

	private DataSlot.BooleanSlot purchasable;
	private DataSlot.BooleanSlot sellable;
	private DataSlot.BooleanSlot purchasableStacked;
	private DataSlot.BooleanSlot sellableStacked;

	public TradeBaseModule(ShopEntry entry, EntryModuleHandler.EntryModuleProvider provider, ArticleSubModule<?> article, CostsSubModule<?> costs) {
		super(provider, entry);

		this.article = article;
		this.costs = costs;
		this.lastTransactions = new HashMap<>();

		if (entry != null) {
			loadData();
		}
	}

	public TradeBaseModule(Map<String, Object> values) {
		super(EntryModuleHandler.getInstance().getProvider((String) values.get("provider")), null);
		this.article = (ArticleSubModule<?>) values.get("article");
		this.costs = (CostsSubModule<?>) values.get("costs");
		this.lastTransactions = new HashMap<>();
	}

	public void setArticle(ArticleSubModule<?> article) {
		this.article = article;
		this.article.loadDataSlots(shopEntry);
	}

	public void setCosts(CostsSubModule<?> costs) {
		this.costs = costs;
		this.costs.loadDataSlots(shopEntry);
	}

	public Price<?> getPayPrice(boolean buy) {
		return buy ? costs.getBuyPrice() : costs.getSellPrice();
	}

	@Override
	public Price<?> getGainPrice() {
		return article.getPrice();
	}

	@Override
	public Component getPriceDisplay(@Nullable Customer customer, boolean buy) {
		return getPriceDisplay(customer, buy, 1.);
	}

	public Component getPriceDisplay(@Nullable Customer customer, boolean buy, double discount) {
		if (buy) {
			return costs.getBuyPrice().toSimplePrice(customer).getPriceComponent(discount);
		}
		return costs.getSellPrice().toSimplePrice(customer).getPriceComponent((discount - 1) * -1 + 1);
	}

	@Override
	public Transaction getLastTransaction(Customer customer) {
		return lastTransactions.get(customer.getUuid());
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		List<DataSlot<?>> data = Lists.newArrayList(purchasable, purchasableStacked, sellable, sellableStacked);
		data.addAll(article.getDataSlots());
		data.addAll(costs.getDataSlots());
		return data.toArray(new DataSlot[0]);
	}

	@Override
	public void loadData() {

		purchasable = shopEntry.getData(DataSlot.BooleanSlot.class, "purchasable", () -> {
			return new DataSlot.BooleanSlot(true);
		});
		sellable = shopEntry.getData(DataSlot.BooleanSlot.class, "sellable", () -> {
			return new DataSlot.BooleanSlot(false);
		});
		purchasableStacked = shopEntry.getData(DataSlot.BooleanSlot.class, "purchasable_stacked", () -> {
			return new DataSlot.BooleanSlot(false);
		});
		sellableStacked = shopEntry.getData(DataSlot.BooleanSlot.class, "sellable_stacked", () -> {
			return new DataSlot.BooleanSlot(false);
		});

		if (article != null) {
			article.loadDataSlots(shopEntry);
		} else {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Tried to load article but article was null");
		}
		if (costs != null) {
			costs.loadDataSlots(shopEntry);
		} else {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Tried to load costs but costs was null");
		}
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		if(!StatShops.getInstance().getShopsConfig().isLogModuleTrade()) {
			return null;
		}
		Transaction t = lastTransactions.get(customer.getUuid());
		if (t != null && result == EntryInteractionResult.SUCCESS) {
			return new LogEntry("customer: '" + customer.getUuid().toString() +
					"', entry: '" + t.getShopEntry().getUUID().toString() +
					"', type: '" + t.getInteractionType().toString().toLowerCase() +
					"', pay: {" + t.getPayPrice().toString() +
					"}, gain: {" + t.getGainPrice().toString() +
					"}, time: '" + t.getLocalDateTime().toString() +
					"', discount: '" + t.getDiscount() +
					"', discounts: {" + t.getAccountedDiscounts().stream().map(discount -> discount.getUuid().toString()).collect(Collectors.joining(",")) +
					"}, limits: not implemented"); //TODO
		}
		return null;
	}

	@Override
	public EntryInteractionResult perform(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {

		if (interactionType.isBuy() && !isPurchasable()) {
			return EntryInteractionResult.FAIL_NOT_PURCHASABLE;
		} else if (interactionType.isSell() && !isSellable()) {
			return EntryInteractionResult.FAIL_NOT_SELLABLE;
		}

		Price<?> pay = (interactionType.isBuy() ? costs.getBuyPrice() : article.getPrice()).toSimplePrice(customer);
		Price<?> gain = (interactionType.isBuy() ? article.getPrice() : costs.getSellPrice()).toSimplePrice(customer);

		List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(customer.getPlayer(), shopEntry, shopEntry.getShop());
		double discount = DiscountHandler.getInstance().combineDiscounts(discounts, interactionType.isSell());

		if (interactionType.isBuy()) {
			pay.applyDiscount(discount);
		} else {
			gain.applyDiscount(discount);
		}

		if (interactionType.equals(EntryInteractionType.BUY_STACK) && isPurchasableStacked()
				|| interactionType.equals(EntryInteractionType.SELL_STACK) && isSellableStacked()) {
			//TODO phu keine ahnung
		}

		if (!gain.canGain(customer)) {
			return EntryInteractionResult.FAIL_CANT_REWARD;
		}
		if (pay.canPay(customer, discount)) {
			if (!LimitsHandler.getInstance().handleLimitInteraction(shopEntry, customer.getPlayer(), menu)) {
				return EntryInteractionResult.FAIL_LIMIT_REACHED;
			}
		}
		EntryInteractionResult result = pay.pay(customer);
		if (result.equals(EntryInteractionResult.SUCCESS)) {
			gain.gain(customer);
			Transaction transaction = new Transaction(customer, getShopEntry(), interactionType, pay, gain, LocalDateTime.now(), discount, discounts);
			lastTransactions.put(customer.getUuid(), transaction);
		}
		return result;
	}

	@Override
	public EntryModule duplicate() {
		return null;
	}

	@Override
	public boolean isPurchasable() {
		return Boolean.TRUE.equals(purchasable.getData());
	}

	@Override
	public void setPurchasable(boolean purchasable) {
		this.purchasable.setData(purchasable);
	}

	@Override
	public boolean isSellable() {
		return Boolean.TRUE.equals(sellable.getData());
	}

	@Override
	public void setSellable(boolean sellable) {
		this.sellable.setData(sellable);
	}

	@Override
	public boolean isPurchasableStacked() {
		return Boolean.TRUE.equals(purchasableStacked.getData());
	}

	@Override
	public void setPurchasableStacked(boolean purchasableStacked) {
		this.purchasableStacked.setData(purchasableStacked);
	}

	@Override
	public boolean isSellableStacked() {
		return Boolean.TRUE.equals(sellableStacked.getData());
	}

	@Override
	public void setSellableStacked(boolean sellableStacked) {
		this.sellableStacked.setData(sellableStacked);
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		HashMap<String, Object> map = new HashMap<>();
		map.put("provider", provider.getKey());
		map.put("article", article);
		map.put("costs", costs);
		return map;
	}
}
