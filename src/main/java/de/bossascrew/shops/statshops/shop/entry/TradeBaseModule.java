package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.EntryModule;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.LogEntry;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.currency.Currency;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import de.bossascrew.shops.statshops.shop.Transaction;
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
public class TradeBaseModule<T> implements TradeModule<T> {

	private ShopEntry shopEntry;
	private final ItemStack displayItem;
	private final Component displayName;
	private final List<Component> displayLore;
	private Currency<T> currency;
	private double priceAmount;
	private T priceObject;
	private ItemStack article;
	private final Map<UUID, Transaction> lastTransactions;

	public TradeBaseModule(Currency<T> currency, double priceAmount, T priceObject, ItemStack article) {
		this.currency = currency;
		this.priceAmount = priceAmount;
		this.priceObject = priceObject;
		this.article = article;
		this.lastTransactions = new HashMap<>();
		displayItem = new ItemStack(Material.EMERALD);
		displayName = Message.GUI_ENTRY_FUNCTION_TRADE_NAME.getTranslation();
		displayLore = Message.GUI_ENTRY_FUNCTION_TRADE_LORE.getTranslations();
	}

	@Override
	public Component getPriceDisplay() {
		return currency.format(priceAmount, priceObject);
	}

	@Override
	public void setPrice(double amount, T object) {
		this.priceAmount = amount;
		this.priceObject = object;
	}

	@Override
	public ItemStack getArticle() {
		return article;
	}

	@Override
	public boolean canGiveArticle(Customer customer) {
		return true;
	}

	@Override
	public void giveArticle(Customer customer) {
		ItemStackUtils.giveOrDrop(customer.getPlayer(), article);
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
	public ShopInteractionResult perform(Customer customer) {
		if (!canGiveArticle(customer)) {
			return ShopInteractionResult.FAIL_CANT_REWARD;
		}
		if (!currency.hasAmount(customer, priceAmount, priceObject)) {
			return ShopInteractionResult.FAIL_CANT_AFFORD;
		}
		currency.removeAmount(customer, priceAmount, priceObject);
		giveArticle(customer);
		Transaction transaction = new Transaction(customer, shopEntry, currency, LocalDateTime.now(), new ArrayList<>()); //TODO discounts
		lastTransactions.put(customer.getUuid(), transaction);
		return ShopInteractionResult.SUCCESS;
	}

	@Override
	public EntryModule duplicate() {
		return null;
	}
}
