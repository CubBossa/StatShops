package de.bossascrew.shops.commands;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.InvalidCommandArgument;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.ItemFlags;
import org.bukkit.Sound;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemFlag;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.stream.Collectors;

@CommandAlias("itemeditor|ieditor|ie")
@Conditions(ShopPlugin.CONDITION_ITEM_IN_HAND + "|" + ShopPlugin.CONDITION_ITEM_HAS_META)
public class ItemEditorCommand extends BaseCommand {

	private void playSuccessSound(Player player) {
		player.playSound(player.getLocation(), Sound.ENTITY_VILLAGER_YES, 1f, 1f);
	}

	private void playFailSound(Player player) {
		player.playSound(player.getLocation(), Sound.ENTITY_VILLAGER_NO, 1f, 1f);
	}

	@Default
	public void onInfo(Player player) {

	}

	@Subcommand("webeditor")
	public void onWebEditor(Player player) {

	}

	@Subcommand("displayname")
	@Description("Set the name of the item in the minimessage format. Type \"null\" to use its default translation component.")
	public void onName(Player player, String name) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		meta.setDisplayName(ComponentUtils.toLegacyFromMiniMessage(name));
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("custommodeldata")
	public void onCustomModelData(Player player, String customModelData) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		try {
			if (customModelData.equalsIgnoreCase("null")) {
				meta.setCustomModelData(null);
			} else {
				meta.setCustomModelData(Integer.parseInt(customModelData));
			}
		} catch (NumberFormatException e) {
			throw new InvalidCommandArgument("CustomModelData must be either a number or \"null\".");
		}
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("lore")
	public void onLoreList(Player player, String lore) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		meta.setLore(Arrays.stream(lore.split("\n")).map(ComponentUtils::toLegacyFromMiniMessage).collect(Collectors.toList()));
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("enchantments add")
	@Syntax("<enchantment> [<level>]")
	@CommandCompletion(ShopPlugin.COMPLETION_ENCHANTMENTS + " 1|2|3|4|5|10")
	public void onEnchant(Player player, Enchantment enchantment, @Nullable Integer level) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		meta.addEnchant(enchantment, level, true);
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("enchantments remove")
	@CommandCompletion(ShopPlugin.COMPLETION_ENCHANTMENTS_CONTAINED)
	public void onEnchant(Player player, Enchantment enchantment) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		if (meta.hasEnchant(enchantment)) {
			meta.removeEnchant(enchantment);
			playSuccessSound(player);
		} else {
			playFailSound(player);
		}
		itemStack.setItemMeta(meta);
	}

	@Subcommand("hideflags")
	@CommandCompletion(ShopPlugin.COMPLETION_ITEM_FLAGS)
	public void onItemFlags(Player player, ItemFlags flags) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		meta.removeItemFlags(ItemFlag.values());
		for (ItemFlag flag : flags.getFlags()) {
			meta.addItemFlags(flag);
		}
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("modifiers")
	public void onItemAttribute(Player player) {

	}

	@Subcommand("repaircost")
	public void onRepairCost(Player player) {

	}

	@Subcommand("damage")
	@Conditions("damagable")
	public void onDamage(Player player) {

	}

	@Subcommand("unbreakable")
	@Conditions("damagable")
	public void onUnbreakable(Player player) {

	}

	@Subcommand("color")
	@Conditions("colorable")
	public void onColor(Player player) {
		// Leather armor, potion, maps

	}

	// Spawners and spawn eggs
	// Cannot handle whole entity creation, so use webinterface for that
	@Subcommand("entity-type")
	@Conditions("spawnable")
	public void onSpawnerEntity(Player player) {

	}


}
