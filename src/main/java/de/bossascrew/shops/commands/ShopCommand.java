package de.bossascrew.shops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ShopManagementMenu;
import de.bossascrew.shops.shop.Shop;
import org.bukkit.entity.Player;

@CommandAlias("shop")
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
			shop.open(Customer.wrap(player), page);
		}
	}

	@Subcommand("open-for")
	@CommandCompletion("@players " + ShopPlugin.COMPLETION_SHOPS + " 1|2|3")
	public void onOpenFor(Player player, Player other, Shop shop, @Optional Integer page) {
		if (page == null) {
			shop.open(Customer.wrap(other));
		} else {
			shop.open(Customer.wrap(other), page);
		}
	}
}
