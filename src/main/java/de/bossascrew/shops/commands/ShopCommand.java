package de.bossascrew.shops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.handler.TranslationHandler;
import de.bossascrew.shops.menu.ShopManagementMenu;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.PaginatedShop;
import de.bossascrew.shops.shop.Shop;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.CompletableFuture;

@CommandAlias("statshops|shops")
public class ShopCommand extends BaseCommand {

	@Default
	public void onDefault(Player player) {
		new ShopManagementMenu().openBaseMenu(player);
	}

	@Subcommand("reload config")
	public void reloadConfig(CommandSender sender) {
		long ms = System.currentTimeMillis();
		CompletableFuture.supplyAsync(() -> ShopPlugin.getInstance().getShopsConfig().loadConfig()).thenAcceptAsync(success -> {
			if (success) {
				ShopPlugin.getInstance().sendMessage(sender, Message.GENERAL_CONFIG_RELOADED_IN_MS.getKey(),
						Message.GENERAL_CONFIG_RELOADED_IN_MS.getTranslation(Template.of("ms", System.currentTimeMillis() - ms + "")), 0);
				return;
			}
			ShopPlugin.getInstance().sendMessage(sender, Message.GENERAL_CONFIG_RELOAD_ERROR);
		});
	}

	@Subcommand("reload language")
	public void reloadTranslations(CommandSender sender) {
		long ms = System.currentTimeMillis();
		TranslationHandler.getInstance().loadLanguage(ShopPlugin.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
			if (success) {
				ShopPlugin.getInstance().sendMessage(sender, Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getKey(),
						Message.GENERAL_LANGUAGE_RELOADED_IN_MS.getTranslation(Template.of("ms", System.currentTimeMillis() - ms + "")), 0);
				return;
			}
			ShopPlugin.getInstance().sendMessage(sender, Message.GENERAL_LANGUAGE_RELOAD_ERROR);
		});
	}

	@Subcommand("open")
	@CommandCompletion(ShopPlugin.COMPLETION_SHOPS + " 1|2|3")
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
	@CommandCompletion("@players " + ShopPlugin.COMPLETION_SHOPS + " 1|2|3")
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
		Bukkit.getScheduler().runTaskTimer(ShopPlugin.getInstance(), () -> {
			try {
				discount.setStartTime(LocalDateTime.now());
				DiscountHandler.getInstance().handleDiscountStart(discount);
			} catch (Throwable t) {
				t.printStackTrace();
			}

		}, 120L, 6 * 20);
	}

}
