package de.bossascrew.shops.itemeditor;

import co.aikar.commands.BaseCommand;
import co.aikar.commands.InvalidCommandArgument;
import co.aikar.commands.annotation.*;
import de.bossascrew.shops.general.util.ItemFlags;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import org.bukkit.Sound;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemFlag;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.Damageable;
import org.bukkit.inventory.meta.ItemMeta;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@CommandAlias("itemeditor|ieditor|ie")
@Conditions(StatShops.CONDITION_ITEM_IN_HAND + "|" + StatShops.CONDITION_ITEM_HAS_META)
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
		meta.setDisplayName(TextUtils.toLegacyFromMiniMessage(name));
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

	@Subcommand("lore add")
	public void onLoreList(Player player, String lore) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		List<String> loreList = meta.getLore();
		if (loreList == null) {
			loreList = new ArrayList<>();
		}
		loreList.addAll(lore.lines()
				.map(TextUtils::toLegacyFromMiniMessage)
				.collect(Collectors.toList()));
		meta.setLore(loreList);
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("lore set")
	public void onLoreListSet(Player player, String lore) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		List<String> loreList = Arrays.stream(lore.split(".*\n.*"))
				.map(TextUtils::toLegacyFromMiniMessage)
				.collect(Collectors.toList());
		meta.setLore(loreList);
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("enchantments add")
	@Syntax("<enchantment> [<level>]")
	@CommandCompletion(StatShops.COMPLETION_ENCHANTMENTS + " 1|2|3|4|5|10")
	public void onEnchant(Player player, Enchantment enchantment, @Nullable Integer level) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		ItemMeta meta = itemStack.getItemMeta();
		meta.addEnchant(enchantment, level, true);
		itemStack.setItemMeta(meta);
		playSuccessSound(player);
	}

	@Subcommand("enchantments remove")
	@CommandCompletion(StatShops.COMPLETION_ENCHANTMENTS_CONTAINED)
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
	@CommandCompletion(StatShops.COMPLETION_ITEM_FLAGS)
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
	@Conditions(StatShops.CONDITION_ITEM_DAMAGABLE)
	@CommandCompletion("0")
	public void onDamage(Player player, int damage) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		if (itemStack.getItemMeta() instanceof Damageable meta) {
			meta.setDamage(damage);
			itemStack.setItemMeta(meta);
			playSuccessSound(player);
		}
	}

	@Subcommand("unbreakable")
	@Conditions(StatShops.CONDITION_ITEM_DAMAGABLE)
	public void onUnbreakable(Player player) {
		ItemStack itemStack = player.getInventory().getItemInMainHand();
		if (itemStack.getItemMeta() instanceof Damageable meta) {
			meta.setUnbreakable(!meta.isUnbreakable());
			itemStack.setItemMeta(meta);
			playSuccessSound(player);
		}
	}

	@Subcommand("color")
	@Conditions(StatShops.CONDITION_ITEM_COLORABLE)
	public void onColor(Player player) {
		// Leather armor, potion, maps

	}

	// Spawners and spawn eggs
	// Cannot handle whole entity creation, so use webinterface for that
	@Subcommand("entity-type")
	@Conditions(StatShops.CONDITION_ITEM_SPAWNABLE)
	public void onSpawnerEntity(Player player) {

	}


}
