package de.bossascrew.shops.shop;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ListMenuElement;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

import java.util.TreeMap;
import java.util.UUID;

@Getter
public class EntryTemplate extends TreeMap<Integer, ShopEntry> implements
		ListMenuElement {

	private final UUID uuid;

	private String nameFormat;
	private String namePlain;
	private Component name;

	public EntryTemplate(UUID uuid, String nameFormat) {
		super();
		this.uuid = uuid;
		setNameFormat(nameFormat);
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = ShopPlugin.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = ComponentUtils.toPlain(this.name);
	}

	@Override
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createTemplatesItemStack(this);
	}
}
