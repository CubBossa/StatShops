package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.TransactionBalanceMessenger;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.util.TradeMessageType;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
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
		List<Price<?>> gain = transaction.getGainPrice().stream()
				.map(price -> price.toSimplePrice(customer)).collect(Collectors.toList());
		List<Price<?>> pay = transaction.getPayPrice().stream()
				.map(price -> price.toSimplePrice(customer))
				.peek(price -> price.setAmount(price.getAmount() * -1))
				.collect(Collectors.toList());

		List<Price<?>> prices = tradeCache.getOrDefault(customer.getUuid(), new ArrayList<>());

		for (Price<?> priceGain : gain) {
			Price<?> cachedGain = prices.stream().filter(price -> price.summable(priceGain)).findAny().orElse(null);
			if (cachedGain == null) {
				prices.addAll(gain);
			} else {
				cachedGain.setAmount(cachedGain.getAmount() + cachedGain.getAmount());
			}
		}
		for (Price<?> pricePay : pay) {
			Price<?> cachedPay = prices.stream().filter(price -> price.summable(pricePay)).findAny().orElse(null);
			if (cachedPay == null) {
				prices.addAll(pay);
			} else {
				cachedPay.setAmount(cachedPay.getAmount() + pricePay.getAmount());
			}
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
			customer.sendMessage(Messages.SHOP_TRADE_FEEDBACK_CUMUL_TITLE, 0);
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

		TagResolver[] templates = {
				TagResolver.resolver("indicator", Tag.inserting(actualAmount >= 0 ?
						Messages.SHOP_TRADE_FEEDBACK_GAIN :
						Messages.SHOP_TRADE_FEEDBACK_PAY)),
				TagResolver.resolver("transaction", Tag.inserting(price.getPriceComponent())),
		};
		if (tradeMessageType.equals(TradeMessageType.PROMPT)) {
			return Messages.SHOP_TRADE_FEEDBACK_PROMPT_FORMAT.asComponent(templates);
		} else {
			return Messages.SHOP_TRADE_FEEDBACK_CUMUL_FORMAT.asComponent(templates);
		}
	}
}
