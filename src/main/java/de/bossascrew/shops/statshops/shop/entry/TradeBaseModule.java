package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.statshops.StatShops;
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

		if (interactionType.isBuy() && !isPurchasable()) {
			return EntryInteractionResult.FAIL_NOT_PURCHASABLE;
		} else if (interactionType.isSell() && !isSellable()) {
			return EntryInteractionResult.FAIL_NOT_SELLABLE;
		}

		Price<?> pay = (interactionType.isBuy() ? buyPayPrice : gainPrice).toSimplePrice();
		Price<?> gain = (interactionType.isBuy() ? gainPrice : sellPayPrice).toSimplePrice();

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

	public static class TradeItemItem extends TradeBaseModule<ItemStack, ItemStack> {

		private DataSlot.ItemStackSlot buyPayPriceItem;
		private DataSlot.ItemStackSlot sellPayPriceItem;
		private DataSlot.ItemStackSlot gainPriceItem;

		private DataSlot.IntegerSlot gainPriceAmount;

		private DataSlot.IntegerSlot buyPayPriceAmount;
		private DataSlot.IntegerSlot sellPayPriceAmount;
		private DataSlot.EquationSlot buyPayEquation;
		private DataSlot.EquationSlot sellPayEquation;

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

			//always int
			data.add(gainPriceAmount);
			if (StatShops.getInstance().getShopsConfig().isDynamicPricingEnabled()) {
				data.add(buyPayEquation);
				data.add(sellPayEquation);
			} else {
				data.add(buyPayPriceAmount);
				data.add(sellPayPriceAmount);
			}
			return data.toArray(new DataSlot[0]);
		}

		@Override
		public void saveData() {
			super.saveData();
			shopEntry.storeData(buyPayPriceItem);
			shopEntry.storeData(sellPayPriceItem);
			shopEntry.storeData(gainPriceItem);

			shopEntry.storeData(gainPriceAmount);
			shopEntry.storeData(buyPayPriceAmount);
			shopEntry.storeData(sellPayPriceAmount);
			shopEntry.storeData(buyPayEquation);
			shopEntry.storeData(sellPayEquation);
		}

		@Override
		public void loadData() {
			super.loadData();
			buyPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "buy_pay_price_item", () -> {
				return new DataSlot.ItemStackSlot("buy_pay_price_item", getBuyPayPrice().getObject(),
						Message.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_LORE);
			});
			buyPayPriceItem.setUpdateHandler(itemStack -> getBuyPayPrice().setObject(itemStack));
			sellPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "sell_pay_price_item", () -> {
				return new DataSlot.ItemStackSlot("sell_pay_price_item", getSellPayPrice().getObject(),
						Message.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_LORE);
			});
			sellPayPriceItem.setUpdateHandler(itemStack -> getSellPayPrice().setObject(itemStack));
			gainPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "gain_price_item", () -> {
				return new DataSlot.ItemStackSlot("gain_price_item", getGainPrice().getObject(),
						Message.GUI_ENTRY_FUNCTION_GAIN_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_GAIN_ITEM_LORE);
			});
			gainPriceItem.setUpdateHandler(itemStack -> getGainPrice().setObject(itemStack));

			gainPriceAmount = shopEntry.getData(DataSlot.IntegerSlot.class, "gain_price_amount", () -> {
				return new DataSlot.IntegerSlot("gain_price_amount", (int) getGainPrice().getAmount(),
						Message.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_LORE);
			});
			gainPriceAmount.setUpdateHandler(integer -> getGainPrice().setAmount(integer));

			buyPayPriceAmount = shopEntry.getData(DataSlot.IntegerSlot.class, "buy_pay_price_amount", () -> {
				return new DataSlot.IntegerSlot("buy_pay_price_amount", 10,
						Message.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_LORE);
			});
			buyPayPriceAmount.setUpdateHandler(integer -> getBuyPayPrice().setAmount(integer));
			sellPayPriceAmount = shopEntry.getData(DataSlot.IntegerSlot.class, "sell_pay_price_amount", () -> {
				return new DataSlot.IntegerSlot("sell_pay_price_amount", 5,
						Message.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_LORE);
			});
			sellPayPriceAmount.setUpdateHandler(integer -> getSellPayPrice().setAmount(integer));

			buyPayEquation = shopEntry.getData(DataSlot.EquationSlot.class, "buy_pay_price_equation", () -> {
				return new DataSlot.EquationSlot("buy_pay_price_equation", "5+5",
						Message.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_LORE);
			});
			buyPayEquation.setUpdateHandler(s -> getBuyPayPrice().setDynamicPriceString(s));
			sellPayEquation = shopEntry.getData(DataSlot.EquationSlot.class, "sell_pay_price_equation", () -> {
				return new DataSlot.EquationSlot("sell_pay_price_equation", "3+2",
						Message.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_LORE);
			});
			buyPayEquation.setUpdateHandler(s -> getSellPayPrice().setDynamicPriceString(s));
		}
	}
}
