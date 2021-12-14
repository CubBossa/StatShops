package de.bossascrew.shops.general.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public interface TradeModule<P, G> extends EntryModule {

	Component getPriceDisplay();

	Transaction getLastTransaction(Customer customer);

	Price<P> getPayPrice();

	Price<G> getGainPrice();
}
