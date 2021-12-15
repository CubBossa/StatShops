package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
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
		return sellPayPrice.getPriceComponent(discount);
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
		//TODO debug
		StatShops.getInstance().log(LoggingPolicy.INFO, customer.getPlayer().getDisplayName() + " interacts: " + "blablabla");
		return new LogEntry();
	}

	@Override
	public EntryInteractionResult perform(Customer customer, EntryInteractionType interactionType) {
		Price<?> pay = interactionType.isBuy() ? buyPayPrice : gainPrice;
		Price<?> gain = interactionType.isBuy() ? gainPrice : sellPayPrice;

		if (!gain.canGain(customer)) {
			return EntryInteractionResult.FAIL_CANT_REWARD;
		}
		List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(shopEntry, shopEntry.getShop());
		double discount = DiscountHandler.getInstance().combineDiscounts(discounts);

		EntryInteractionResult result = pay.pay(customer, interactionType.isBuy() ? discount : 1);
		if (result.equals(EntryInteractionResult.SUCCESS)) {
			gain.gain(customer, interactionType.isBuy() ? 1 : discount);
			Transaction transaction = new Transaction(customer, getShopEntry(), interactionType,
					interactionType.isBuy() ? buyPayPrice : sellPayPrice, gainPrice, LocalDateTime.now(), discount, discounts);
			lastTransactions.put(customer.getUuid(), transaction);
		}
		return result;
	}

	@Override
	public EntryModule duplicate() {
		return null;
	}
}
