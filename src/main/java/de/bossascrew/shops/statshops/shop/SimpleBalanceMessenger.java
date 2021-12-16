package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.TransactionBalanceMessenger;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.util.TradeMessageType;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.entity.Player;

import java.util.*;
import java.util.stream.Collectors;

public class SimpleBalanceMessenger implements TransactionBalanceMessenger {

	@Getter
	@Setter
	private TradeMessageType tradeMessageType;
	private final Map<UUID, List<Price<?>>> tradeCache;

	public SimpleBalanceMessenger(TradeMessageType tradeMessageType) {
		this.tradeMessageType = tradeMessageType;
		this.tradeCache = new HashMap<>();
	}


	@Override
	public void handleTransaction(Transaction transaction) {
		if (tradeMessageType.equals(TradeMessageType.NONE)) {
			return;
		}

		Customer customer = transaction.getCustomer();
		TradeModule<?, ?> tm = (TradeModule<?, ?>) transaction.getShopEntry().getModule();
		Price<?> gain = tm.getGainPrice().duplicate();
		Price<?> pay = tm.getPayPrice(transaction.getInteractionType().isBuy()).duplicate();
		gain.setAmount((int) (gain.getAmount() * transaction.getDiscount()) * (transaction.getInteractionType().isBuy() ? 1 : -1));
		pay.setAmount((int) (pay.getAmount() * transaction.getDiscount()) * (transaction.getInteractionType().isBuy() ? -1 : 1));

		List<Price<?>> prices = tradeCache.getOrDefault(customer.getUuid(), new ArrayList<>());

		Price<?> cachedGain = prices.stream().filter(price -> price.equals(gain)).findAny().orElse(null);
		if (cachedGain == null) {
			prices.add(gain);
		} else {
			cachedGain.setAmount(cachedGain.getAmount() + gain.getAmount());
		}
		Price<?> cachedPay = prices.stream().filter(price -> price.equals(pay)).findAny().orElse(null);
		if (cachedPay == null) {
			prices.add(pay);
		} else {
			cachedPay.setAmount(cachedPay.getAmount() + pay.getAmount());
		}
		tradeCache.put(customer.getUuid(), prices);

		TradeMessageType feedback = StatShops.getInstance().getShopsConfig().getTradeMessageFeedback();
		if (feedback.equals(TradeMessageType.PROMPT)) {
			printCachedBalanceAndClear(customer, false);
		}
	}

	@Override
	public void handlePageClose(Player player) {
		if (tradeMessageType.equals(TradeMessageType.CUMULATIVE_PAGE)) {
			printCachedBalanceAndClear(Customer.wrap(player), true);
		}
	}

	@Override
	public void handleShopClose(Player player) {
		if (tradeMessageType.equals(TradeMessageType.CUMULATIVE_SHOP)) {
			printCachedBalanceAndClear(Customer.wrap(player), true);
		}
	}

	private void printCachedBalanceAndClear(Customer customer, boolean header) {
		List<Price<?>> cache = tradeCache.get(customer.getUuid());
		tradeCache.put(customer.getUuid(), new ArrayList<>());
		if (cache == null || cache.stream().noneMatch(price -> price.getAmount() != 0)) {
			return;
		}
		if (header) {
			customer.sendMessage(Message.SHOP_TRADE_FEEDBACK_CUMUL_TITLE, 0);
		}
		cache = cache.stream().sorted().collect(Collectors.toList());
		for (Price<?> price : cache) {
			if (price.getAmount() == 0) {
				continue;
			}
			customer.sendMessage("", getTransactionFeedback(price), 0);
		}
	}

	private Component getTransactionFeedback(Price<?> price) {
		double actualAmount = price.getAmount();
		price = price.duplicate();
		price.setAmount(Math.abs(price.getAmount()));

		Template[] templates = {
				Template.of("indicator", actualAmount >= 0 ?
						Message.SHOP_TRADE_FEEDBACK_GAIN.getTranslation() :
						Message.SHOP_TRADE_FEEDBACK_PAY.getTranslation()),
				Template.of("transaction", price.getPriceComponent()),
		};
		if (tradeMessageType.equals(TradeMessageType.PROMPT)) {
			return Message.SHOP_TRADE_FEEDBACK_PROMPT_FORMAT.getTranslation(templates);
		} else {
			return Message.SHOP_TRADE_FEEDBACK_CUMUL_FORMAT.getTranslation(templates);
		}
	}
}
