package de.bossascrew.shops.general;

import de.bossascrew.shops.statshops.shop.Transaction;
import org.bukkit.entity.Player;

/**
 * Is responsible for caching and sending a balance sheet to the shopping player depending on the triggers and configurations.
 * The default implementation is {@link de.bossascrew.shops.statshops.shop.SimpleBalanceMessenger}
 */
public interface TransactionBalanceMessenger {

	void handleTransaction(Transaction transaction);

	void handlePageClose(Player player);

	void handleShopClose(Player player);
}
