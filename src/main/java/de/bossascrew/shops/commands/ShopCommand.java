package de.bossascrew.shops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.menu.ShopManagementMenu;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.PaginatedShop;
import de.bossascrew.shops.shop.Shop;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

@CommandAlias("statshops|shops")
public class ShopCommand extends BaseCommand {

	@Default
	public void onDefault(Player player) {
		new ShopManagementMenu().openBaseMenu(player);
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
			discount.setStartTime(LocalDateTime.now());
			DiscountHandler.getInstance().handleDiscountStart(discount);
		}, 120L, 6 * 20);
	}
}
