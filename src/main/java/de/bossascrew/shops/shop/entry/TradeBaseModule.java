package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.LogEntry;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.shop.Currency;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.util.ItemStackUtils;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.List;

@Getter
@Setter
public class TradeBaseModule<T> implements TradeModule<T> {

	private final ItemStack displayItem;
	private final Component displayName;
	private final List<Component> displayLore;
	private Currency<T> currency;
	private double priceAmount;
	private T priceObject;
	private ItemStack article;

	public TradeBaseModule(Currency<T> currency, double priceAmount, T priceObject, ItemStack article) {
		this.currency = currency;
		this.priceAmount = priceAmount;
		this.priceObject = priceObject;
		this.article = article;
		displayItem = new ItemStack(Material.EMERALD);
		displayName = Message.MANAGER_GUI_ENTRY_FUNCTION_TRADE_NAME.getTranslation();
		displayLore = Message.MANAGER_GUI_ENTRY_FUNCTION_TRADE_LORE.getTranslations();
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
		ShopPlugin.getInstance().log(LoggingPolicy.INFO, customer.getPlayer().getDisplayName() + " interacts: " + "blablabla");
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
		return ShopInteractionResult.SUCCESS;
	}
}
