package de.bossascrew.shops.util;

import lombok.Getter;
import org.bukkit.inventory.ItemFlag;

import java.util.ArrayList;
import java.util.List;

public class ItemFlags {

	@Getter
	private final List<ItemFlag> flags;

	public ItemFlags(ItemFlag... flags) {
		this.flags = new ArrayList<>();
		this.flags.addAll(List.of(flags));
	}

	public boolean add(ItemFlag itemFlag) {
		return flags.add(itemFlag);
	}
}
