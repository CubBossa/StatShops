package de.bossascrew.shops.statshops;

import de.bossascrew.shops.statshops.handler.CurrencyHandler;
import de.bossascrew.shops.statshops.handler.DynamicPricingHandler;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.api.data.DefaultPricingDatabase;
import de.bossascrew.shops.statshops.handler.TranslationHandler;
import lombok.Getter;
import org.bukkit.plugin.java.JavaPlugin;
import org.jetbrains.annotations.Nullable;

/**
 * A class that allows to add currencies, trade submodules and database types to StatShops.
 * Since the extension is registered before StatShops will be enabled, StatShops will load all currencies etc.
 * in the right order. <br>This prevents loaded data which refers to e.g. a currency that has not been registered at that point.
 */
public abstract class StatShopsExtension {

	@Getter
	private final @Nullable JavaPlugin plugin;

	public StatShopsExtension(JavaPlugin plugin) {
		this.plugin = plugin;
	}

	/**
	 * Register the currencies for your extension. See {@link de.bossascrew.shops.statshops.shop.currency.Currency} for
	 * documentation on currencies or {@link de.bossascrew.shops.statshops.hook.VaultExtension} for an example.
	 *
	 * @param currencyHandler the instance of the currency handler to register the currencies at.
	 */
	public void registerCurrencies(CurrencyHandler currencyHandler) {

	}

	/**
	 * Register all messages that are supposed to be loaded from the language.yml.
	 * A {@link de.bossascrew.shops.statshops.data.Message} requires a key that also servers as path in the yml. You may want to choose keys that
	 * start with your plugin name (like my-gem-plugin.[...]). The language.yml will then read all messages
	 * from this configuration section.
	 * <p>
	 * If you don't want to use translatable messages and have hardcoded messages, you can ignore this method.
	 *
	 * @param translationHandler The TranslationHandler instance to register the messages at.
	 *                           <p>Use {@code translationHandler.registerMessages(Message... messages);}
	 */
	public void registerMessages(TranslationHandler translationHandler) {

	}

	/**
	 * Allows to provide a different database. If you, for example, want to use a database type that hasn't been implemented
	 * by StatShops or want to do some changes on the way customers are stored, you can provide a database type here.
	 * <br>Make sure that you also enable it in the config.
	 */
	public void provideDatabase() { //TODO databasehandler

	}

	/**
	 * Allows to provide a default pricing by registering an instance of {@link DefaultPricingDatabase}
	 * <br><br>You can do this by calling the method loadDefaultPricing like so: {@code handler.loadDefaultPricing("essentials", new EssentialsWorthReader())}
	 *
	 * @param handler the singleton instance of a {@link DynamicPricingHandler}
	 */
	public void registerDynamicPricingDefaultValues(DynamicPricingHandler handler) {

	}

	/**
	 * By overriding this method, you can provide more entry modules. Entry modules are the types that a shop entry can be, like next_page, prev_page, close, static.
	 * Trade entry modules are automatically generated when registering a {@link SubModulesHandler.ArticleSubModuleProvider}.
	 * You will register a provider that contains messages and the display item for the admin control panel. Once registered, it will show up in the control panel
	 * and can be applied by the user (server staff).
	 *
	 * @param entryModuleHandler the singleton instance of {@link EntryModuleHandler}
	 */
	public void registerEntryModules(EntryModuleHandler entryModuleHandler) {

	}

	/**
	 * You can register multiple {@link SubModulesHandler.ArticleSubModuleProvider} instances, that will automatically be displayed in the admin control panel
	 * as trade modules. (register a command ArticuleSubModuleProvider and it will show up as a "trade command module" in the modules panel of the gui)
	 * {@link SubModulesHandler.CostsSubModuleProvider} instead are only displayed when the module of an entry is a {@link de.bossascrew.shops.statshops.api.module.TradeModule}.
	 * The gui will display a seperate button where the user can pick between cost types (pay with items/money/xp/gems/...)
	 *
	 * @param subModulesHandler the singleton instance of {@link SubModulesHandler}
	 */
	public void registerTradeSubModules(SubModulesHandler subModulesHandler) {

	}

	//TODO other  shoptypes to shophandler
}
