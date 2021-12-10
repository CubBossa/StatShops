package de.bossascrew.shops.util;

import de.bossascrew.shops.shop.Taggable;
import lombok.experimental.UtilityClass;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.SpawnEggMeta;

import java.util.*;

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

	public boolean hasCommonTags(Taggable a, Taggable b) {
		for (String s : a.getTags()) {
			if (b.hasTag(s)) {
				return true;
			}
		}
		return false;
	}

	public Collection<String> getTags(ItemStack itemStack, boolean material, boolean groups) {
		Collection<String> tags = getTags(itemStack.getType(), material, groups);
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
