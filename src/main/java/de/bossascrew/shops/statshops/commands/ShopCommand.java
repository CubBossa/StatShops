package de.bossascrew.shops.statshops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.PaginatedShop;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.handler.InventoryHandler;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.TranslationHandler;
import de.bossascrew.shops.statshops.menu.ShopManagementMenu;
import de.bossascrew.shops.statshops.shop.Discount;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.CompletableFuture;

@CommandAlias("statshops|shops|shop")
public class ShopCommand extends BaseCommand {

	@Default
	public void onDefault(Player player) {
		if (StatShops.busy()) {
			Customer.wrap(player).sendMessage(Message.GENERAL_PLUGIN_LOADING);
			return;
		}
		new ShopManagementMenu().openBaseMenu(player);
	}

	@Subcommand("reload config")
	public void reloadConfig(CommandSender sender) {
		if (StatShops.busy()) {
			StatShops.getInstance().sendMessage(sender, Message.GENERAL_PLUGIN_LOADING);
			return;
		}
		long ms = System.currentTimeMillis();
		InventoryHandler.getInstance().closeAllMenus(false);

		StatShops.setBusyFor(CompletableFuture.supplyAsync(() -> StatShops.getInstance().getShopsConfig().loadConfig()).thenAcceptAsync(success -> {
			if (success) {
				StatShops.getInstance().sendMessage(sender, Message.GENERAL_CONFIG_RELOADED_IN_MS.getKey(),
						Message.GENERAL_CONFIG_RELOADED_IN_MS.getTranslation(Template.of("ms", System.currentTimeMillis() - ms + "")), 0);
				return;
			}
			StatShops.getInstance().sendMessage(sender, Message.GENERAL_CONFIG_RELOAD_ERROR);
		}));
	}

	@Subcommand("reload language")
	public void reloadTranslations(CommandSender sender) {
		if (StatShops.busy()) {
			StatShops.getInstance().sendMessage(sender, Message.GENERAL_PLUGIN_LOADING);
			return;
		}
		long ms = System.currentTimeMillis();
		StatShops.setBusyFor(TranslationHandler.getInstance().loadLanguage(StatShops.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
			if (success) {
				StatShops.getInstance().sendMessage(sender, Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getKey(),
						Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getTranslation(Template.of("ms", System.currentTimeMillis() - ms + "")), 0);
				return;
			}
			StatShops.getInstance().sendMessage(sender, Message.GENERAL_LANGUAGE_RELOAD_ERROR);
		}));
	}

	@Subcommand("open")
	@CommandCompletion(StatShops.COMPLETION_SHOPS + " 1|2|3")
	public void onOpen(Player player, Shop shop, @Optional Integer page) {
		if (page == null) {
			shop.open(Customer.wrap(player));
		} else {
			if (shop instanceof PaginatedShop ps) {
				ps.open(Customer.wrap(player), page);
			} else {
				shop.open(Customer.wrap(player));
			}
		}
	}

	@Subcommand("open-for")
	@CommandCompletion("@players " + StatShops.COMPLETION_SHOPS + " 1|2|3")
	public void onOpenFor(Player player, Player other, Shop shop, @Optional Integer page) {
		if (page == null) {
			shop.open(Customer.wrap(other));
		} else {
			if (shop instanceof PaginatedShop ps) {
				ps.open(Customer.wrap(player), page);
			} else {
				shop.open(Customer.wrap(player));
			}
		}
	}

	@Subcommand("test")
	public void onTest(CommandSender player) {

		Discount discount = DiscountHandler.getInstance().createDiscount("<rainbow>Test Discount", LocalDateTime.now(), Duration.of(3, ChronoUnit.SECONDS), 0.5, "test");
		discount.addTag("test");
		Bukkit.getScheduler().runTaskTimer(StatShops.getInstance(), () -> {
			try {
				discount.addStartTime(LocalDateTime.now());
				DiscountHandler.getInstance().handleDiscountStart(discount);
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}, 120L, 6 * 20);
	}

}
