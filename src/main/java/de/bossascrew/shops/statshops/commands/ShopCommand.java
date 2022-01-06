package de.bossascrew.shops.statshops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.data.NamedObject;
import de.bossascrew.shops.statshops.convertion.DataPreset;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.InventoryHandler;
import de.bossascrew.shops.statshops.handler.TranslationHandler;
import de.bossascrew.shops.statshops.menu.ShopManagementMenu;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.HoverEvent;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.Container;
import org.bukkit.block.DoubleChest;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;

import java.io.File;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@CommandAlias("statshop|statshops|shop|shops")
public class ShopCommand extends BaseCommand {

	private final UUID consoleUuid = UUID.randomUUID();
	private final Map<UUID, Runnable> confirmingPlayers;

	public ShopCommand() {
		confirmingPlayers = new HashMap<>();
	}

	@Subcommand("accept")
	public void accept(CommandSender sender) {
		Runnable r;
		if (sender instanceof Player player) {
			r = confirmingPlayers.get(player.getUniqueId());
		} else {
			r = confirmingPlayers.get(consoleUuid);
		}
		if (r != null) {
			r.run();
		}
	}

	@Subcommand("decline")
	public void decline(CommandSender sender) {
		if (sender instanceof Player player) {
			confirmingPlayers.remove(player.getUniqueId());
		} else {
			confirmingPlayers.remove(consoleUuid);
		}
	}

	@Subcommand("debug")
	public class Debug extends BaseCommand {
		@Subcommand("entries")
		@CommandCompletion(StatShops.COMPLETION_SHOPS)
		public void onEntries(CommandSender sender, Shop shop) {
			shop.getEntries().forEach((integer, entry) -> System.out.println(integer + " -> " + entry.getDisplayItem().getType()));
		}
	}

	@Subcommand("export")
	public void onExport(CommandSender sender, String name) {
		CompletableFuture.supplyAsync(() ->  {
			DataPreset dataPreset = new DataPreset(name, "CubBossa", StatShops.getInstance().getDescription().getVersion(), LocalDateTime.now());
			dataPreset.loadFromCacheByTags();
			return dataPreset;

		}).thenAccept(dataPreset -> {
			StatShops.getInstance().sendMessage(sender, Message.DATA_PRESET_EXPORT_CONFIRM.getTranslation(
					Template.of("shops", listElements(dataPreset.getShops(), new Pair<>("Shop", "Shops"))),
					Template.of("discounts", listElements(dataPreset.getDiscounts(), new Pair<>("Discount", "Discounts"))),
					Template.of("limits", listElements(dataPreset.getLimits(), new Pair<>("Limit", "Limits"))),
					Template.of("templates", listElements(dataPreset.getTemplates(), new Pair<>("Template", "Templates"))),
					Template.of("name", dataPreset.getName())
			));
			UUID uuid = sender instanceof Player player ? player.getUniqueId() : consoleUuid;
			confirmingPlayers.put(uuid, () -> {
				dataPreset.toFile();
				StatShops.getInstance().sendMessage(sender, Message.DATA_PRESET_EXPORT_SUCCESS.getTranslation(Template.of("name", dataPreset.getName())));
				confirmingPlayers.remove(uuid);
			});
		});
	}

	@Subcommand("import")
	@CommandCompletion(StatShops.COMPLETION_DATA_TEMPLATES)
	public void onImport(CommandSender sender, String fileName) {
		CompletableFuture.supplyAsync(() -> {
			return new DataPreset(new File(StatShops.getInstance().getDataFolder(), "presets/" + fileName));

		}).thenAccept(dataPreset -> {
			StatShops.getInstance().sendMessage(sender, Message.DATA_PRESET_IMPORT_CONFIRM.getTranslation(
					Template.of("shops", listElements(dataPreset.getShops(), new Pair<>("Shop", "Shops"))),
					Template.of("discounts", listElements(dataPreset.getDiscounts(), new Pair<>("Discount", "Discounts"))),
					Template.of("limits", listElements(dataPreset.getLimits(), new Pair<>("Limit", "Limits"))),
					Template.of("templates", listElements(dataPreset.getTemplates(), new Pair<>("Template", "Templates"))),
					Template.of("name", dataPreset.getName() + " (author: " + dataPreset.getAuthor() + ")")
			));
			UUID uuid = sender instanceof Player player ? player.getUniqueId() : consoleUuid;
			confirmingPlayers.put(uuid, () -> {
				dataPreset.apply();
				StatShops.getInstance().sendMessage(sender, Message.DATA_PRESET_IMPORT_SUCCESS.getTranslation(Template.of("name", dataPreset.getName())));
				confirmingPlayers.remove(uuid);
			});
		});
	}

	private Component listElements(Collection<? extends NamedObject> objects, Pair<String, String> name) {
		Component c = Component.empty();
		for (var o : objects) {
			c = c.append(o.getName()).append(Component.text(", ", NamedTextColor.GRAY));
		}
		return Component.text(objects.size() + " " + (objects.size() == 1 ? name.getLeft() : name.getRight())).hoverEvent(HoverEvent.showText(c));
	}

	@Default
	@Subcommand("editor")
	@CommandPermission(StatShops.PERM_CMD_EDITOR)
	public void onDefault(Player player) {
		if (StatShops.busy()) {
			Customer.wrap(player).sendMessage(Message.GENERAL_PLUGIN_LOADING);
			return;
		}
		new ShopManagementMenu().openBaseMenu(player);
	}

	@Subcommand("edit")
	@Syntax("<Shop>")
	@CommandPermission(StatShops.PERM_CMD_EDITOR)
	@CommandCompletion(StatShops.COMPLETION_SHOPS)
	public void onEdit(Player player, Shop shop) {
		if (StatShops.busy()) {
			Customer.wrap(player).sendMessage(Message.GENERAL_PLUGIN_LOADING);
			return;
		}
		new ShopManagementMenu().openShopMenu(player, shop, 0);
	}

	@Subcommand("cleanup")
	@Description("Removes unused shop entries from the shop")
	@Syntax("<Shop>")
	@CommandCompletion(StatShops.COMPLETION_SHOPS)
	public void onCleanUp(CommandSender sender, Shop shop) {
		shop.cleanupUnusedEntries();
		//TODO feedback und so
	}

	@Subcommand("reload config")
	@CommandPermission(StatShops.PERM_CMD_RELOAD)
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
	@CommandPermission(StatShops.PERM_CMD_RELOAD)
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
	@CommandPermission(StatShops.PERM_CMD_OPEN)
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
	@CommandPermission(StatShops.PERM_CMD_OPEN_FOR)
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

	@Subcommand("parse-chest")
	public void onParseChest(Player player, Shop shop, @Optional Integer page) {
		page = page == null ? 0 : page;

		if (!(shop instanceof ChestMenuShop)) {
			player.sendMessage("Falscher shop");
			//TODO
			return;
		}
		Block block = player.getTargetBlockExact(10);
		if (block.getState() instanceof Container container) {
			Inventory inv = container.getSnapshotInventory();
			if (container.getInventory().getHolder() instanceof DoubleChest doubleChest) {
				inv = doubleChest.getLeftSide().getInventory();
			}
			int count = parseInventory(shop, page * RowedOpenableMenu.LARGEST_INV_SIZE, inv);

			Customer.wrap(player).sendMessage(Message.GENERAL_CHEST_PARSED.getKey(),
					Message.GENERAL_CHEST_PARSED.getTranslation(Template.of("amount", count + ""), Template.of("shop", shop.getName())));
		} else {
			player.sendMessage("Kein container");
			//TODO
		}
	}

	private int parseInventory(Shop shop, int offset, Inventory inv) {
		int count = 0;
		for (int i = 0; i < inv.getSize(); i++) {
			ItemStack stack = inv.getItem(i);
			if (stack == null || stack.getType() == Material.AIR) {
				continue;
			}
			shop.createEntry(stack, offset + i);
			count++;
		}
		return count;
	}
}
