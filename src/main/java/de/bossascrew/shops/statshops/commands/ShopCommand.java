package de.bossascrew.shops.statshops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.data.NamedObject;
import de.bossascrew.shops.statshops.convertion.DataPreset;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.handler.TranslationHandler;
import de.bossascrew.shops.statshops.menu.MainMenu;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.HoverEvent;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Bukkit;
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

		@Subcommand("tags")
		@CommandCompletion(StatShops.COMPLETION_SHOPS)
		public void onTags(CommandSender sender, Shop shop) {
			shop.getTags(true).forEach(System.out::println);
		}
	}

	@Subcommand("export")
	public void onExport(CommandSender sender, String name) {
		CompletableFuture.supplyAsync(() ->  {
			DataPreset dataPreset = new DataPreset(name, "CubBossa", StatShops.getInstance().getDescription().getVersion(), LocalDateTime.now());
			dataPreset.loadFromCacheByTags();
			return dataPreset;

		}).thenAccept(dataPreset -> {
			StatShops.getInstance().sendMessage(sender, Messages.DATA_PRESET_EXPORT_CONFIRM.asComponent(
					TagResolver.resolver("shops", Tag.inserting(listElements(dataPreset.getShops(), new Pair<>("Shop", "Shops")))),
					TagResolver.resolver("discounts", Tag.inserting(listElements(dataPreset.getDiscounts(), new Pair<>("Discount", "Discounts")))),
					TagResolver.resolver("limits", Tag.inserting(listElements(dataPreset.getLimits(), new Pair<>("Limit", "Limits")))),
					TagResolver.resolver("templates", Tag.inserting(listElements(dataPreset.getTemplates(), new Pair<>("Template", "Templates")))),
					TagResolver.resolver("name", Tag.inserting(Component.text(dataPreset.getName())))
			));
			UUID uuid = sender instanceof Player player ? player.getUniqueId() : consoleUuid;
			confirmingPlayers.put(uuid, () -> {
				dataPreset.toFile();
				StatShops.getInstance().sendMessage(sender, Messages.DATA_PRESET_EXPORT_SUCCESS.asComponent(TagResolver.resolver("name", Tag.inserting(Component.text(dataPreset.getName())))));
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
			StatShops.getInstance().sendMessage(sender, Messages.DATA_PRESET_IMPORT_CONFIRM.asComponent(
					TagResolver.resolver("shops", Tag.inserting(listElements(dataPreset.getShops(), new Pair<>("Shop", "Shops")))),
					TagResolver.resolver("discounts", Tag.inserting(listElements(dataPreset.getDiscounts(), new Pair<>("Discount", "Discounts")))),
					TagResolver.resolver("limits", Tag.inserting(listElements(dataPreset.getLimits(), new Pair<>("Limit", "Limits")))),
					TagResolver.resolver("templates", Tag.inserting(listElements(dataPreset.getTemplates(), new Pair<>("Template", "Templates")))),
					TagResolver.resolver("name", Tag.inserting(Component.text(dataPreset.getName() + " (author: " + dataPreset.getAuthor() + ")")))
			));
			UUID uuid = sender instanceof Player player ? player.getUniqueId() : consoleUuid;
			confirmingPlayers.put(uuid, () -> {
				dataPreset.apply();
				StatShops.getInstance().sendMessage(sender, Messages.DATA_PRESET_IMPORT_SUCCESS.asComponent(TagResolver.resolver("name", Tag.inserting(Component.text(dataPreset.getName())))));
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
			Customer.wrap(player).sendMessage(Messages.GENERAL_PLUGIN_LOADING);
			return;
		}
		MainMenu.openBaseMenu(player);
	}

	@Subcommand("edit")
	@Syntax("<Shop>")
	@CommandPermission(StatShops.PERM_CMD_EDITOR)
	@CommandCompletion(StatShops.COMPLETION_SHOPS)
	public void onEdit(Player player, Shop shop) {
		if (StatShops.busy()) {
			Customer.wrap(player).sendMessage(Messages.GENERAL_PLUGIN_LOADING);
			return;
		}
		MainMenu.newShopMenu(shop, player).open(player);
	}

	@Subcommand("cleanup")
	@Description("Removes unused shop entries from the shop")
	@Syntax("<Shop>")
	@CommandCompletion(StatShops.COMPLETION_SHOPS)
	public void onCleanUp(CommandSender sender, Shop shop) {
		StatShops.getInstance().sendMessage(sender, Messages.GUI_SHOP_EDITOR_REMOVED_UNUSED.getKey(),
				Messages.GUI_SHOP_EDITOR_REMOVED_UNUSED.asComponent(
						TagResolver.resolver("amount", Tag.inserting(Component.text(shop.cleanupUnusedEntries()))),
						TagResolver.resolver("shop", Tag.inserting(shop.getName()))
				), 0);
	}

	@Subcommand("reload config")
	@CommandPermission(StatShops.PERM_CMD_RELOAD)
	public void reloadConfig(CommandSender sender) {
		if (StatShops.busy()) {
			StatShops.getInstance().sendMessage(sender, Messages.GENERAL_PLUGIN_LOADING);
			return;
		}
		long ms = System.currentTimeMillis();
		Bukkit.getOnlinePlayers().forEach(Player::closeInventory);

		StatShops.setBusyFor(CompletableFuture.supplyAsync(() -> StatShops.getInstance().getShopsConfig().loadConfig()).thenAcceptAsync(success -> {
			if (success) {
				StatShops.getInstance().sendMessage(sender, Messages.GENERAL_CONFIG_RELOADED_IN_MS.getKey(),
						Messages.GENERAL_CONFIG_RELOADED_IN_MS.asComponent(TagResolver.resolver("ms", Tag.inserting(Component.text(System.currentTimeMillis() - ms)))), 0);
				return;
			}
			StatShops.getInstance().sendMessage(sender, Messages.GENERAL_CONFIG_RELOAD_ERROR);
		}));
	}

	@Subcommand("reload language")
	@CommandPermission(StatShops.PERM_CMD_RELOAD)
	public void reloadTranslations(CommandSender sender) {
		if (StatShops.busy()) {
			StatShops.getInstance().sendMessage(sender, Messages.GENERAL_PLUGIN_LOADING);
			return;
		}
		long ms = System.currentTimeMillis();
		StatShops.setBusyFor(TranslationHandler.getInstance().loadLanguage(StatShops.getInstance().getShopsConfig().getLanguage()).thenAcceptAsync(success -> {
			if (success) {
				StatShops.getInstance().sendMessage(sender, Messages.GENERAL_LANGUAGE_RELOADED_IN_MS.getKey(),
						Messages.GENERAL_LANGUAGE_RELOADED_IN_MS.asComponent(TagResolver.resolver("ms", Tag.inserting(Component.text(System.currentTimeMillis() - ms)))), 0);
				return;
			}
			StatShops.getInstance().sendMessage(sender, Messages.GENERAL_LANGUAGE_RELOAD_ERROR);
		}));
	}

	@Subcommand("open")
	@CommandPermission(StatShops.PERM_CMD_OPEN)
	@CommandCompletion(StatShops.COMPLETION_SHOPS + " 1|2|3")
	public void onOpen(Player player, Shop shop, @Optional Integer page) {
		if (page != null && shop instanceof PaginatedShop ps) {
			ps.open(Customer.wrap(player), page);
		} else {
			shop.open(Customer.wrap(player));
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
		page = page == null || !(shop instanceof PaginatedShop) ? 0 : page;

		Block block = player.getTargetBlockExact(10);
		if (block.getState() instanceof Container container) {
			Inventory inv = container.getSnapshotInventory();
			if (container.getInventory().getHolder() instanceof DoubleChest doubleChest) {
				inv = doubleChest.getLeftSide().getInventory();
			}
			int count = parseInventory(shop, page * (9 * 6), inv);

			Customer.wrap(player).sendMessage(Messages.GENERAL_CHEST_PARSED.getKey(),
					Messages.GENERAL_CHEST_PARSED.asComponent(
							TagResolver.resolver("amount", Tag.inserting(Component.text(count))),
							TagResolver.resolver("shop", Tag.inserting(shop.getName()))));
		} else {
			Customer.wrap(player).sendMessage(Messages.GENERAL_NO_CHEST);
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
