package de.bossascrew.shops.statshops.shop.entry;

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
import de.bossascrew.shops.statshops.shop.currency.DynamicPrice;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.shop.currency.SimplePrice;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.*;

@Getter
@Setter
public class TradeBaseModule<P, G> extends BaseModule implements TradeModule<P, G> {

	private Price<P> buyPayPrice;
	private Price<P> sellPayPrice;
	private Price<G> gainPrice;
	private final Map<UUID, Transaction> lastTransactions;

	private DataSlot.BooleanSlot purchasable;
	private DataSlot.BooleanSlot sellable;
	private DataSlot.BooleanSlot purchasableStacked;
	private DataSlot.BooleanSlot sellableStacked;

	public TradeBaseModule(ShopEntry entry, EntryModuleHandler.EntryModuleProvider provider, Price<P> payPrice, Price<G> gainPrice) {
		this(entry, provider, payPrice, payPrice, gainPrice);
	}

	public TradeBaseModule(ShopEntry entry, EntryModuleHandler.EntryModuleProvider provider, Price<P> buyPayPrice, Price<P> sellPayPrice, Price<G> gainPrice) {
		super(provider, entry);

		this.buyPayPrice = buyPayPrice;
		this.sellPayPrice = sellPayPrice;
		this.gainPrice = gainPrice;
		this.lastTransactions = new HashMap<>();

		loadData();
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
		return new DataSlot[]{
				purchasable, purchasableStacked,
				sellable, sellableStacked
		};
	}

	@Override
	public void loadData() {
		purchasable = shopEntry.getData(DataSlot.BooleanSlot.class, "purchasable", () -> {
			return new DataSlot.BooleanSlot("purchasable", true, Message.GUI_ENTRY_FUNCTION_PURCHASABLE);
		});
		sellable = shopEntry.getData(DataSlot.BooleanSlot.class, "sellable", () -> {
			return new DataSlot.BooleanSlot("sellable", false, Message.GUI_ENTRY_FUNCTION_SELLABLE);
		});
		purchasableStacked = shopEntry.getData(DataSlot.BooleanSlot.class, "purchasable_stacked", () -> {
			return new DataSlot.BooleanSlot("purchasable_stacked", false, Message.GUI_ENTRY_FUNCTION_PURCHASABLE_STACKED);
		});
		sellableStacked = shopEntry.getData(DataSlot.BooleanSlot.class, "sellable_stacked", () -> {
			return new DataSlot.BooleanSlot("sellable_stacked", false, Message.GUI_ENTRY_FUNCTION_SELLABLE_STACKED);
		});
	}

	@Override
	public void saveData() {
		shopEntry.storeData(purchasable);
		shopEntry.storeData(sellable);
		shopEntry.storeData(purchasableStacked);
		shopEntry.storeData(sellableStacked);
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

	public static class TradeItemItem extends TradeBaseModule<ItemStack, ItemStack> {

		private DataSlot.ItemStackSlot buyPayPriceItem;
		private DataSlot.ItemStackSlot sellPayPriceItem;
		private DataSlot.ItemStackSlot gainPriceItem;

		public TradeItemItem(ShopEntry entry, EntryModuleHandler.EntryModuleProvider provider, Price<ItemStack> payPrice, Price<ItemStack> gainPrice) {
			this(entry, provider, payPrice, payPrice.duplicate(), gainPrice);
		}

		public TradeItemItem(ShopEntry entry, EntryModuleHandler.EntryModuleProvider provider, Price<ItemStack> buyPayPrice, Price<ItemStack> sellPayPrice, Price<ItemStack> gainPrice) {
			super(entry, provider, buyPayPrice, sellPayPrice, gainPrice);
		}

		@Override
		public DataSlot<?>[] getDataSlots() {
			List<DataSlot<?>> data = new ArrayList<>(Arrays.asList(super.getDataSlots()));
			data.add(buyPayPriceItem);
			data.add(sellPayPriceItem);
			data.add(gainPriceItem);
			return data.toArray(new DataSlot[0]);
		}

		@Override
		public void saveData() {
			super.saveData();
			shopEntry.storeData(buyPayPriceItem);
			shopEntry.storeData(sellPayPriceItem);
			shopEntry.storeData(gainPriceItem);
		}

		@Override
		public void loadData() {
			super.loadData();
			buyPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "buy_pay_price", () -> {
				return new DataSlot.ItemStackSlot("buy_pay_price", getBuyPayPrice().getObject(), Message.NONE);
			});
			buyPayPriceItem.setUpdateHandler(itemStack -> getBuyPayPrice().setObject(itemStack));
			sellPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "sell_pay_price", () -> {
				return new DataSlot.ItemStackSlot("sell_pay_price", getSellPayPrice().getObject(), Message.NONE);
			});
			sellPayPriceItem.setUpdateHandler(itemStack -> getSellPayPrice().setObject(itemStack));
			gainPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "gain_price", () -> {
				return new DataSlot.ItemStackSlot("gain_price", getGainPrice().getObject(), Message.NONE); //TODO messages
			});
			gainPriceItem.setUpdateHandler(itemStack -> getGainPrice().setObject(itemStack));
		}
	}
}
