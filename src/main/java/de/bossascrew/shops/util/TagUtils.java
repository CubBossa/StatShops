package de.bossascrew.shops.util;

import de.bossascrew.shops.shop.Taggable;
import lombok.experimental.UtilityClass;
import org.bukkit.Material;
import org.bukkit.Tag;

import java.util.ArrayList;
import java.util.List;

@UtilityClass
public class TagUtils {

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

	public List<String> getTags(Material material) {
		List<String> tags = new ArrayList<>();
		return tags;
	}
}
