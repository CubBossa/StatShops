package de.bossascrew.shops.statshops.shop.entry;

import com.google.common.collect.Lists;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.CurrencyHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.shop.currency.Price;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ArticleSubModule<T> implements SubModule {

	@Getter
	private SubModulesHandler.ArticleSubModuleProvider<T> provider;

	private DataSlot.NumberSlot gainPriceAmount;
	@Getter
	private final Price<T> price;

	public ArticleSubModule(SubModulesHandler.ArticleSubModuleProvider<T> provider, Price<T> price) {
		this.provider = provider;
		this.price = price;
	}

	public ArticleSubModule(Map<String, Object> values) {
		this.provider = (SubModulesHandler.ArticleSubModuleProvider<T>) SubModulesHandler.getInstance().getArticleProvider((String) values.get("provider"));
		this.price = (Price<T>) values.get("price");
	}

	public List<DataSlot<?>> getDataSlots() {
		return Lists.newArrayList(gainPriceAmount);
	}

	public void loadDataSlots(ShopEntry shopEntry) {
		gainPriceAmount = shopEntry.getData(DataSlot.NumberSlot.class, "gain_price_amount", () -> {
			return new DataSlot.NumberSlot(getPrice().getAmount());
		});
		gainPriceAmount.setUpdateHandler(integer -> getPrice().setAmount(integer));
	}

	@NotNull
	@Override
	public Map<String, Object> serialize() {
		return Map.of("provider", provider.getKey(), "price", price);
	}

	public static class ItemArticle extends ArticleSubModule<ItemStack> {

		DataSlot.ItemStackSlot gainPriceItem;

		public ItemArticle(SubModulesHandler.ArticleSubModuleProvider<ItemStack> provider) {
			super(provider, new Price<>(CurrencyHandler.CURRENCY_ITEM, 1., new ItemStack(Material.DIRT, 1))); //TODO zu viele default values hew
		}

		public ItemArticle(Map<String, Object> values) {
			super(values);
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
				return new DataSlot.ItemStackSlot(shopEntry.getDisplayItem());
			});
			gainPriceItem.setUpdateHandler(itemStack -> getPrice().setObject(itemStack));
			gainPriceItem.getUpdateHandler().accept(shopEntry.getDisplayItem());
		}
	}

	public static class CommandArticle extends ArticleSubModule<String> {

		public CommandArticle(SubModulesHandler.ArticleSubModuleProvider<String> provider) { //TODO command dataslot
			super(provider, new Price<>(CurrencyHandler.CURRENCY_COMMAND, 1, "help"));
		}

		public CommandArticle(Map<String, Object> values) {
			super(values);
		}
	}

	public static class ConsoleCommandArticle extends ArticleSubModule<String> {

		public ConsoleCommandArticle(SubModulesHandler.ArticleSubModuleProvider<String> provider) { //TODO command dataslot
			super(provider, new Price<>(CurrencyHandler.CURRENCY_CONSOLE_COMMAND, 1, "help"));
		}

		public ConsoleCommandArticle(Map<String, Object> values) {
			super(values);
		}
	}
}
