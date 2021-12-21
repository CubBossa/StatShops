package de.bossascrew.shops.statshops.shop.entry;

import com.google.common.collect.Lists;
import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

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

		loadData();
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
	public Component getPriceDisplay(boolean buy) {
		return getPriceDisplay(buy, 1.);
	}

	public Component getPriceDisplay(boolean buy, double discount) {
		if (buy) {
			return costs.getBuyPrice().getPriceComponent(discount);
		}
		return costs.getSellPrice().getPriceComponent((discount - 1) * -1 + 1);
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
			return new DataSlot.BooleanSlot("purchasable", true,
					Message.GUI_ENTRY_FUNCTION_PURCHASABLE_NAME, Message.GUI_ENTRY_FUNCTION_PURCHASABLE_LORE);
		});
		sellable = shopEntry.getData(DataSlot.BooleanSlot.class, "sellable", () -> {
			return new DataSlot.BooleanSlot("sellable", false,
					Message.GUI_ENTRY_FUNCTION_SELLABLE_NAME, Message.GUI_ENTRY_FUNCTION_SELLABLE_LORE);
		});
		purchasableStacked = shopEntry.getData(DataSlot.BooleanSlot.class, "purchasable_stacked", () -> {
			return new DataSlot.BooleanSlot("purchasable_stacked", false,
					Message.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_NAME, Message.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED_LORE);
		});
		sellableStacked = shopEntry.getData(DataSlot.BooleanSlot.class, "sellable_stacked", () -> {
			return new DataSlot.BooleanSlot("sellable_stacked", false,
					Message.GUI_ENTRY_FUNCTION_SELLABLE_STACKED_NAME, Message.GUI_ENTRY_FUNCTION_SELLABLE_STACKED_LORE);
		});

		article.loadDataSlots(shopEntry);
		costs.loadDataSlots(shopEntry);
	}

	@Override
	public void saveData() {
		shopEntry.storeData(purchasable);
		shopEntry.storeData(sellable);
		shopEntry.storeData(purchasableStacked);
		shopEntry.storeData(sellableStacked);

		article.saveDataSlots(shopEntry);
		costs.saveDataSlots(shopEntry);
	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		//TODO log
		return new LogEntry();
	}

	@Override
	public EntryInteractionResult perform(Customer customer, EntryInteractionType interactionType) {

		if (interactionType.isBuy() && !isPurchasable()) {
			return EntryInteractionResult.FAIL_NOT_PURCHASABLE;
		} else if (interactionType.isSell() && !isSellable()) {
			return EntryInteractionResult.FAIL_NOT_SELLABLE;
		}

		Price<?> pay = (interactionType.isBuy() ? costs.getBuyPrice() : article.getPrice()).toSimplePrice();
		Price<?> gain = (interactionType.isBuy() ? article.getPrice() : costs.getSellPrice()).toSimplePrice();

		List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(shopEntry, shopEntry.getShop());
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
}
