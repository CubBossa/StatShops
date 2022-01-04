package de.bossascrew.shops.statshops.util;

import com.google.common.collect.Multimap;
import de.bossascrew.shops.statshops.api.Taggable;
import lombok.experimental.UtilityClass;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.attribute.Attribute;
import org.bukkit.attribute.AttributeModifier;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.PotionMeta;

import java.util.*;
import java.util.stream.Collectors;

@UtilityClass
public class TagUtils {

	private final Map<Material, Collection<String>> cache = new HashMap<>();

	public List<String> getDoubleTags(Taggable a, Taggable b) {
		List<String> tags = new ArrayList<>();
		for (String s : a.getTags()) {
			if (b.hasTag(s)) {
				tags.add(s);
			}
		}
		return tags;
	}

	public boolean hasCommonTags(Taggable a, Collection<String> list) {
		for (String s : a.getTags()) {
			if (list.contains(s)) {
				return true;
			}
		}
		return false;
	}

	public boolean hasCommonTags(Taggable a, Taggable b) {
		for (String s : a.getTags()) {
			if (b.hasTag(s)) {
				return true;
			}
		}
		return false;
	}

	public Collection<String> getTags(ItemStack itemStack, boolean material, boolean groups, boolean enchantments, boolean potions, boolean attributes) {
		Collection<String> tags = getTags(itemStack.getType(), material, groups);

		if (enchantments) {
			if (itemStack.getItemMeta() instanceof EnchantmentStorageMeta meta) {
				tags.addAll(meta.getEnchants().keySet().stream().map(e -> e.getKey().getKey()).collect(Collectors.toList()));
			}
			tags.addAll(itemStack.getEnchantments().keySet().stream().map(e -> e.getKey().getKey()).collect(Collectors.toList()));
		}
		if (potions) {
			if (itemStack.getItemMeta() instanceof PotionMeta meta) {
				if (meta.hasCustomEffects()) {
					tags.addAll(meta.getCustomEffects().stream().map(effect -> effect.getType().getName().toLowerCase()).collect(Collectors.toList()));
				}
			}
		}
		if (attributes) {
			if (itemStack.hasItemMeta()) {
				Multimap<Attribute, AttributeModifier> map = itemStack.getItemMeta().getAttributeModifiers();
				if (map != null) {
					tags.addAll(map.keySet().stream().map(attribute -> attribute.getKey().toString()).collect(Collectors.toList()));
				}
			}
		}
		return tags;
	}

	public Collection<String> getTags(Material material, boolean self, boolean groups) {
		if (cache.containsKey(material)) {
			return cache.get(material);
		}
		List<String> tags = new ArrayList<>();
		if(groups) {
			Bukkit.getTags("blocks", Material.class).forEach(materialTag -> {
				if (materialTag.isTagged(material)) {
					tags.add(materialTag.getKey().getKey());
				}
			});
		}
		if(self) {
			tags.add(material.toString().toLowerCase());
		}
		cache.put(material, tags);
		return tags;
	}
}
