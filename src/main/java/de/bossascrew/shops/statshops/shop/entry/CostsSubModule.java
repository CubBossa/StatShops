package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.CurrencyHandler;
import de.bossascrew.shops.general.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

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

	public CostsSubModule(Map<String, Object> values) throws ClassCastException {
		this((SubModulesHandler.CostsSubModuleProvider<T>) SubModulesHandler.getInstance().getCostsProvider((String) values.get("provider")),
				(Price<T>) values.get("buy-price"), (Price<T>) values.get("sell-price"));
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

	public void setCosts(String buyPayEquation, String sellPayEquation) {
		this.buyPayEquation.setData(buyPayEquation);
		this.sellPayEquation.setData(sellPayEquation);
	}

	public void setCosts(double buyPayAmount, double sellPayAmount) {
		this.buyPayPriceAmount.setData(buyPayAmount);
		this.sellPayPriceAmount.setData(sellPayAmount);
	}

	public void loadDataSlots(ShopEntry shopEntry) {

		buyPayPriceAmount = shopEntry.getData(DataSlot.NumberSlot.class, "buy_pay_price_amount", () -> {
			return new DataSlot.NumberSlot(10.);
		});
		buyPayPriceAmount.setUpdateHandler(integer -> getBuyPrice().setAmount(integer));
		sellPayPriceAmount = shopEntry.getData(DataSlot.NumberSlot.class, "sell_pay_price_amount", () -> {
			return new DataSlot.NumberSlot(5.);
		});
		sellPayPriceAmount.setUpdateHandler(integer -> getSellPrice().setAmount(integer));

		buyPayEquation = shopEntry.getData(DataSlot.EquationSlot.class, "buy_pay_price_equation", () -> {
			return new DataSlot.EquationSlot("5+5");
		});
		buyPayEquation.setUpdateHandler(s -> getBuyPrice().setDynamicPriceString(s, buyPayPriceAmount.getData()));
		sellPayEquation = shopEntry.getData(DataSlot.EquationSlot.class, "sell_pay_price_equation", () -> {
			return new DataSlot.EquationSlot("3+2");
		});
		sellPayEquation.setUpdateHandler(s -> getSellPrice().setDynamicPriceString(s, sellPayPriceAmount.getData()));
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		return Map.of("provider", provider.getKey(), "sell-price", sellPrice, "buy-price", buyPrice);
	}

	public static class ItemCosts extends CostsSubModule<ItemStack> {

		private DataSlot.ItemStackSlot buyPayPriceItem;
		private DataSlot.ItemStackSlot sellPayPriceItem;

		public ItemCosts(SubModulesHandler.CostsSubModuleProvider<ItemStack> provider) {
			super(provider, new Price<>(CurrencyHandler.CURRENCY_ITEM, 5, new ItemStack(Material.EMERALD)),
					new Price<>(CurrencyHandler.CURRENCY_ITEM, 3, new ItemStack(Material.EMERALD)));
		}

		public ItemCosts(Map<String, Object> values) {
			super(values);
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
				return new DataSlot.ItemStackSlot(getBuyPrice().getObject());
			});
			buyPayPriceItem.setUpdateHandler(itemStack -> getBuyPrice().setObject(itemStack));
			sellPayPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "sell_pay_price_item", () -> {
				return new DataSlot.ItemStackSlot(getSellPrice().getObject());
			});
			sellPayPriceItem.setUpdateHandler(itemStack -> getSellPrice().setObject(itemStack));
		}
	}

	public static class ExpCosts extends CostsSubModule<Void> {

		public ExpCosts(SubModulesHandler.CostsSubModuleProvider<Void> provider) {
			super(provider, new Price<>(CurrencyHandler.CURRENCY_EXP, 100, null),
					new Price<>(CurrencyHandler.CURRENCY_EXP, 80, null));
		}

		public ExpCosts(Map<String, Object> values) {
			super(values);
		}
	}
}
