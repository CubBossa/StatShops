package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.TransactionBalanceMessenger;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.general.util.TradeMessageType;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class SimpleBalanceMessenger implements TransactionBalanceMessenger {

	@Getter
	@Setter
	private TradeMessageType tradeMessageType;
	private final Map<UUID, Map<Component, Double>> tradeCache;

	public SimpleBalanceMessenger(TradeMessageType tradeMessageType) {
		this.tradeMessageType = tradeMessageType;
		this.tradeCache = new HashMap<>();
	}


	@Override
	public void handleTransaction(Transaction transaction) {
		if (tradeMessageType.equals(TradeMessageType.NONE)) {
			return;
		}

		//TODO hier viel zeug nach currency und reward auslagern
		Customer customer = transaction.getCustomer();
		TradeModule<ItemStack> tm = (TradeModule<ItemStack>) transaction.getShopEntry().getModule();

		double priceAmount = tm.getPriceAmount() * -1;
		int gainAmount = tm.getArticle().getAmount();

		TradeMessageType feedback = StatShops.getInstance().getShopsConfig().getTradeMessageFeedback();
		Map<Component, Double> innerMap = tradeCache.getOrDefault(customer, new HashMap<>());

		Component gainComponent = TextUtils.toComponent(tm.getArticle());
		double setGain = innerMap.getOrDefault(gainComponent, 0.);

		Component priceComponent = tm.getCurrency().getCurrencyComponent(2, tm.getPriceObject());
		double setPrice = innerMap.getOrDefault(priceComponent, 0.);

		innerMap.put(gainComponent, setGain + gainAmount);
		innerMap.put(priceComponent, setPrice + priceAmount);

		if (feedback.equals(TradeMessageType.PROMPT)) {
			printCachedBalanceAndClear(customer, false);
		}
		tradeCache.put(customer.getUuid(), innerMap);
	}

	@Override
	public void handlePageClose(Player player) {
		if (tradeMessageType.equals(TradeMessageType.CUMULATIVE_PAGE)) {
			printCachedBalanceAndClear(Customer.wrap(player));
		}
	}

	@Override
	public void handleShopClose(Player player) {
		if (tradeMessageType.equals(TradeMessageType.CUMULATIVE_SHOP)) {
			printCachedBalanceAndClear(Customer.wrap(player));
		}
	}

	private void printCachedBalanceAndClear(Customer customer) {
		printCachedBalanceAndClear(customer, tradeCache.getOrDefault(customer.getUuid(), new HashMap<>()).size() > 0);
	}

	private void printCachedBalanceAndClear(Customer customer, boolean header) {
		if (header) {
			customer.sendMessage(Message.SHOP_TRADE_FEEDBACK_CUMUL_TITLE, 0);
		}
		for (Map.Entry<Component, Double> entry : tradeCache.getOrDefault(customer.getUuid(), new HashMap<>()).entrySet()) {
			customer.sendMessage("", getTransactionFeedback(entry.getValue(), entry.getKey(), false), 0);
		}
		tradeCache.put(customer.getUuid(), new HashMap<>());
	}

	private Component getTransactionFeedback(double amount, Component tradeObjectComponent, boolean toInt) { //TODO toint aus currency holen
		Template[] templates = {
				Template.of("indicator", amount >= 0 ? Message.SHOP_TRADE_FEEDBACK_GAIN.getTranslation() : Message.SHOP_TRADE_FEEDBACK_PAY.getTranslation()),
				Template.of("amount", (toInt ? "" + ((int) Math.abs(amount)) : "" + Math.abs(amount))),
				Template.of("subject", tradeObjectComponent)
		};
		if (tradeMessageType.equals(TradeMessageType.PROMPT)) {
			return Message.SHOP_TRADE_FEEDBACK_PROMPT_FORMAT.getTranslation(templates);
		} else {
			return Message.SHOP_TRADE_FEEDBACK_CUMUL_FORMAT.getTranslation(templates);
		}
	}
}
