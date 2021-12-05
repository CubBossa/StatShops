package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.LogEntry;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.shop.Currency;
import de.bossascrew.shops.shop.ShopInteractionResult;
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

	public TradeBaseModule(Currency<T> currency, double priceAmount, T priceObject) {
		this.currency = currency;
		this.priceAmount = priceAmount;
		this.priceObject = priceObject;
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
	public void giveArticle(Customer customer) {

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
	public @Nullable LogEntry createLogEntry() {
		return new LogEntry();
	}

	@Override
	public ShopInteractionResult perform(Customer customer) {
		return null; //TODO
	}
}
