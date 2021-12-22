package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.CurrencyHandler;
import de.bossascrew.shops.general.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class CostsSubModule<T> implements SubModule {

	@Getter
	private SubModulesHandler.CostsSubModuleProvider<T> provider;

	private DataSlot.NumberSlot buyPayPriceAmount;
	private DataSlot.NumberSlot sellPayPriceAmount;
	private DataSlot.EquationSlot buyPayEquation;
	private DataSlot.EquationSlot sellPayEquation;

	@Getter
	private Price<T> buyPrice;
	@Getter
	private Price<T> sellPrice;

	public CostsSubModule(SubModulesHandler.CostsSubModuleProvider<T> provider, Price<T> buyPrice, Price<T> sellPrice) {
		this.provider = provider;
		this.buyPrice = buyPrice;
		this.sellPrice = sellPrice;
	}

	public List<DataSlot<?>> getDataSlots() {
		List<DataSlot<?>> data = new ArrayList<>();
		if (StatShops.getInstance().getShopsConfig().isDynamicPricingEnabled()) {
			data.add(buyPayEquation);
			data.add(sellPayEquation);
		} else {
			data.add(buyPayPriceAmount);
			data.add(sellPayPriceAmount);
		}
		return data;
	}

	public void loadDataSlots(ShopEntry shopEntry) {

		buyPayPriceAmount = shopEntry.getData(DataSlot.NumberSlot.class, "buy_pay_price_amount", () -> {
			return new DataSlot.NumberSlot("buy_pay_price_amount", 10.,
					Message.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_AMOUNT_LORE);
		});
		buyPayPriceAmount.setUpdateHandler(integer -> getBuyPrice().setAmount(integer));
		sellPayPriceAmount = shopEntry.getData(DataSlot.NumberSlot.class, "sell_pay_price_amount", () -> {
			return new DataSlot.NumberSlot("sell_pay_price_amount", 5.,
					Message.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_AMOUNT_LORE);
		});
		sellPayPriceAmount.setUpdateHandler(integer -> getSellPrice().setAmount(integer));

		buyPayEquation = shopEntry.getData(DataSlot.EquationSlot.class, "buy_pay_price_equation", () -> {
			return new DataSlot.EquationSlot("buy_pay_price_equation", "5+5",
					Message.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_EQUATION_LORE);
		});
		buyPayEquation.setUpdateHandler(s -> getBuyPrice().setDynamicPriceString(s));
		sellPayEquation = shopEntry.getData(DataSlot.EquationSlot.class, "sell_pay_price_equation", () -> {
			return new DataSlot.EquationSlot("sell_pay_price_equation", "3+2",
					Message.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_EQUATION_LORE);
		});
		buyPayEquation.setUpdateHandler(s -> getSellPrice().setDynamicPriceString(s));
	}

	public void saveDataSlots(ShopEntry shopEntry) {

		shopEntry.storeData(buyPayPriceAmount);
		shopEntry.storeData(sellPayPriceAmount);
		shopEntry.storeData(buyPayEquation);
		shopEntry.storeData(sellPayEquation);
	}

	public static class ItemCosts extends CostsSubModule<ItemStack> {

		private DataSlot.ItemStackSlot buyPayPriceItem;
		private DataSlot.ItemStackSlot sellPayPriceItem;

		public ItemCosts(SubModulesHandler.CostsSubModuleProvider<ItemStack> provider) {
			super(provider, new Price<>(CurrencyHandler.CURRENCY_ITEM, 5, new ItemStack(Material.EMERALD)),
					new Price<>(CurrencyHandler.CURRENCY_ITEM, 3, new ItemStack(Material.EMERALD)));
		}

		@Override
		public List<DataSlot<?>> getDataSlots() {
			List<DataSlot<?>> data = super.getDataSlots();
			data.add(buyPayPriceItem);
			data.add(sellPayPriceItem);
			return data;
		}

		@Override
		public void loadDataSlots(ShopEntry shopEntry) {
			super.loadDataSlots(shopEntry);

			buyPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "buy_pay_price_item", () -> {
				return new DataSlot.ItemStackSlot("buy_pay_price_item", getBuyPrice().getObject(),
						Message.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_BUY_PRICE_ITEM_LORE);
			});
			buyPayPriceItem.setUpdateHandler(itemStack -> getBuyPrice().setObject(itemStack));
			sellPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "sell_pay_price_item", () -> {
				return new DataSlot.ItemStackSlot("sell_pay_price_item", getSellPrice().getObject(),
						Message.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_SELL_PRICE_ITEM_LORE);
			});
			sellPayPriceItem.setUpdateHandler(itemStack -> getSellPrice().setObject(itemStack));
		}

		@Override
		public void saveDataSlots(ShopEntry shopEntry) {
			super.saveDataSlots(shopEntry);
			shopEntry.storeData(buyPayPriceItem);
			shopEntry.storeData(sellPayPriceItem);
		}
	}

	public static class ExpCosts extends CostsSubModule<Void> {

		public ExpCosts(SubModulesHandler.CostsSubModuleProvider<Void> provider) {
			super(provider, new Price<>(CurrencyHandler.CURRENCY_EXP, 100, null),
					new Price<>(CurrencyHandler.CURRENCY_EXP, 80, null));
		}
	}
}
