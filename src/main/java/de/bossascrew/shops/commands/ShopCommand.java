package de.bossascrew.shops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.Customer;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ShopManagementMenu;
import de.bossascrew.shops.menu.VillagerMenu;
import de.bossascrew.shops.shop.PaginatedShop;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.util.LoggingPolicy;
import net.kyori.adventure.text.Component;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

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
	public void onTest(Player player) {
		VillagerMenu villagerMenu = new VillagerMenu(Component.text("lol"), null);
		villagerMenu.setMerchantOffer(0, new ItemStack(Material.DIAMOND), new ItemStack(Material.EMERALD));
		villagerMenu.setMerchantOffer(1, new ItemStack(Material.EMERALD), new ItemStack(Material.REDSTONE));
		villagerMenu.setMerchantOffer(2, new ItemStack(Material.SAND, 32), new ItemStack(Material.EMERALD));
		for (int i = 0; i < 65; i++) {
			villagerMenu.setMerchantOffer(i, new ItemStack(Material.SAND, i), new ItemStack(Material.EMERALD, i));
		}


		villagerMenu.setTradeHandler(clickContext -> ShopPlugin.getInstance().log(LoggingPolicy.INFO, clickContext.getSlot() + ""));
		villagerMenu.setTradeSelectHandler(clickContext -> {
			ShopPlugin.getInstance().log(LoggingPolicy.INFO, "Huii selected: " + clickContext.getSlot());
		});
		villagerMenu.openInventory(player);
	}
}
