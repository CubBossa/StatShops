package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.DynamicPrice;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.currency.SimplePrice;
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
public class TradeBaseModule<P, G> extends BaseModule implements TradeModule<P, G> {

	private boolean buyable = true;
	private boolean sellable = true;
	private boolean buyableStacked = true;
	private boolean sellableStacked = true;

	private Price<P> buyPayPrice;
	private Price<P> sellPayPrice;
	private Price<G> gainPrice;
	private final Map<UUID, Transaction> lastTransactions;

	public TradeBaseModule(EntryModuleHandler.EntryModuleProvider provider, Price<P> payPrice, Price<G> gainPrice) {
		this(provider, payPrice, payPrice, gainPrice);
	}

	public TradeBaseModule(EntryModuleHandler.EntryModuleProvider provider, Price<P> buyPayPrice, Price<P> sellPayPrice, Price<G> gainPrice) {
		super(provider, null);

		this.buyPayPrice = buyPayPrice;
		this.sellPayPrice = sellPayPrice;
		this.gainPrice = gainPrice;
		this.lastTransactions = new HashMap<>();

	}

	public Price<P> getPayPrice(boolean buy) {
		return buy ? buyPayPrice : sellPayPrice;
	}

	@Override
	public Component getPriceDisplay(boolean buy) {
		return getPriceDisplay(buy, 1.);
	}

	public Component getPriceDisplay(boolean buy, double discount) {
		if (buy) {
			return buyPayPrice.getPriceComponent(discount);
		}
		return sellPayPrice.getPriceComponent((discount - 1) * -1 + 1);
	}

	@Override
	public Transaction getLastTransaction(Customer customer) {
		return lastTransactions.get(customer.getUuid());
	}

	@Override
	public DataSlot<?>[] getDataSlots() {
		return new DataSlot[0];
	}

	@Override
	public void loadData() {

	}

	@Override
	public void saveData() {

	}

	@Override
	public @Nullable LogEntry createLogEntry(Customer customer, EntryInteractionResult result) {
		//TODO log
		return new LogEntry();
	}

	@Override
	public EntryInteractionResult perform(Customer customer, EntryInteractionType interactionType) {
		Price<?> _pay = interactionType.isBuy() ? buyPayPrice : gainPrice;
		Price<?> _gain = interactionType.isBuy() ? gainPrice : sellPayPrice;
		SimplePrice<?> pay = _pay instanceof DynamicPrice dpay ? dpay.toSimplePrice() : (SimplePrice<?>) _pay.duplicate();
		SimplePrice<?> gain = _gain instanceof DynamicPrice dgain ? dgain.toSimplePrice() : (SimplePrice<?>) _gain.duplicate();

		List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(shopEntry, shopEntry.getShop());
		double discount = DiscountHandler.getInstance().combineDiscounts(discounts, interactionType.isSell());

		if (interactionType.isBuy()) {
			pay.applyDiscount(discount);
		} else {
			gain.applyDiscount(discount);
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
}
