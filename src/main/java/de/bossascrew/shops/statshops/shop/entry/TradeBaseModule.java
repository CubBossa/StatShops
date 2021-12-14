package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.util.EntryInteractionType;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import de.bossascrew.shops.statshops.shop.Transaction;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDateTime;
import java.util.*;

@Getter
@Setter
public class TradeBaseModule<P, G> implements TradeModule<P, G> {

	private ShopEntry shopEntry;
	private final ItemStack displayItem;
	private final Component displayName;
	private final List<Component> displayLore;

	private boolean buyEnabled = true;
	private boolean sellEnabled = true;

	private Price<P> payPrice; //TODO einmal für sell einmal für buy
	private Price<G> gainPrice;
	private final Map<UUID, Transaction> lastTransactions;

	public TradeBaseModule(Price<P> payPrice, Price<G> gainPrice) {

		this.payPrice = payPrice;
		this.gainPrice = gainPrice;
		this.lastTransactions = new HashMap<>();

		displayItem = new ItemStack(Material.EMERALD); //TODO was macht ihr hier?
		displayName = Message.GUI_ENTRY_FUNCTION_TRADE_NAME.getTranslation();
		displayLore = Message.GUI_ENTRY_FUNCTION_TRADE_LORE.getTranslations();
	}

	@Override
	public Component getPriceDisplay() {
		return payPrice.getPriceComponent();
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
	public @Nullable LogEntry createLogEntry(Customer customer, ShopInteractionResult result) {
		//TODO debug
		StatShops.getInstance().log(LoggingPolicy.INFO, customer.getPlayer().getDisplayName() + " interacts: " + "blablabla");
		return new LogEntry();
	}

	@Override
	public ShopInteractionResult perform(Customer customer, EntryInteractionType interactionType) {
		Price<?> pay = interactionType.isBuy() ? payPrice : gainPrice;
		Price<?> gain = interactionType.isBuy() ? gainPrice : payPrice;

		if (!gain.canGain(customer)) {
			return ShopInteractionResult.FAIL_CANT_REWARD;
		}
		ShopInteractionResult result = pay.pay(customer);
		if (result.equals(ShopInteractionResult.SUCCESS)) {
			gain.gain(customer);
			Transaction transaction = new Transaction(customer, shopEntry, interactionType, payPrice, gainPrice, LocalDateTime.now(), new ArrayList<>()); //TODO discounts
			lastTransactions.put(customer.getUuid(), transaction);
		}
		return result;
	}

	@Override
	public EntryModule duplicate() {
		return null;
	}
}
