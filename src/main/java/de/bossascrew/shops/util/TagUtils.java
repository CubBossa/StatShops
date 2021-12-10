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

	public Collection<String> getTags(ItemStack itemStack) {
		Collection<String> tags = getTags(itemStack.getType());
		return tags;
	}

	public Collection<String> getTags(Material material) {
		if (cache.containsKey(material)) {
			return cache.get(material);
		}
		List<String> tags = new ArrayList<>();
		Bukkit.getTags("blocks", Material.class).forEach(materialTag -> {
			if (materialTag.isTagged(material)) {
				tags.add(materialTag.getKey().getKey());
			}
		});
		tags.add(material.toString().toLowerCase());
		cache.put(material, tags);
		return tags;
	}
}
