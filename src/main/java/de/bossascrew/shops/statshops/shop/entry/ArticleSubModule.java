package de.bossascrew.shops.statshops.shop.entry;

import com.google.common.collect.Lists;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.CurrencyHandler;
import de.bossascrew.shops.general.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;

import java.util.ArrayList;
import java.util.List;

public class ArticleSubModule<T> implements SubModule {

	@Getter
	private SubModulesHandler.ArticleSubModuleProvider<T> provider;

	DataSlot.NumberSlot gainPriceAmount;
	@Getter
	private Price<T> price;

	public ArticleSubModule(SubModulesHandler.ArticleSubModuleProvider<T> provider, Price<T> price) {
		this.provider = provider;
		this.price = price;
	}

	public List<DataSlot<?>> getDataSlots() {
		return Lists.newArrayList(gainPriceAmount);
	}

	public void loadDataSlots(ShopEntry shopEntry) {
		gainPriceAmount = shopEntry.getData(DataSlot.NumberSlot.class, "gain_price_amount", () -> {
			return new DataSlot.NumberSlot("gain_price_amount", getPrice().getAmount(),
					Message.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_NAME, Message.GUI_ENTRY_FUNCTION_GAIN_AMOUNT_LORE);
		});
		gainPriceAmount.setUpdateHandler(integer -> getPrice().setAmount(integer));
	}

	public void saveDataSlots(ShopEntry shopEntry) {
		shopEntry.storeData(gainPriceAmount);
	}

	public static class ItemArticle extends ArticleSubModule<ItemStack> {

		DataSlot.ItemStackSlot gainPriceItem;

		public ItemArticle(SubModulesHandler.ArticleSubModuleProvider<ItemStack> provider) {
			super(provider, new Price<>(CurrencyHandler.CURRENCY_ITEM, 5., new ItemStack(Material.DIRT, 1))); //TODO zu viele default values hew
		}

		@Override
		public List<DataSlot<?>> getDataSlots() {
			List<DataSlot<?>> data = new ArrayList<>(super.getDataSlots());
			data.add(gainPriceItem);
			return data;
		}

		@Override
		public void loadDataSlots(ShopEntry shopEntry) {
			super.loadDataSlots(shopEntry);

			gainPriceItem = shopEntry.getData(DataSlot.ItemStackSlot.class, "gain_price_item", () -> {
				return new DataSlot.ItemStackSlot("gain_price_item", shopEntry.getDisplayItem(),
						Message.GUI_ENTRY_FUNCTION_GAIN_ITEM_NAME, Message.GUI_ENTRY_FUNCTION_GAIN_ITEM_LORE);
			});
			gainPriceItem.setUpdateHandler(itemStack -> getPrice().setObject(itemStack));
			gainPriceItem.getUpdateHandler().accept(shopEntry.getDisplayItem());
		}

		@Override
		public void saveDataSlots(ShopEntry shopEntry) {
			super.saveDataSlots(shopEntry);
			shopEntry.storeData(gainPriceItem);
		}
	}

	public static class CommandArticle extends ArticleSubModule<String> {

		public CommandArticle(SubModulesHandler.ArticleSubModuleProvider<String> provider) { //TODO command dataslot
			super(provider, new Price<>(CurrencyHandler.CURRENCY_COMMAND, 1, "help"));
		}
	}
}
