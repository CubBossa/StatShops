package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;

@Getter
@Setter
public abstract class BaseModule {

	protected EntryModuleHandler.EntryModuleProvider provider;
	protected ShopEntry shopEntry;

	public BaseModule(EntryModuleHandler.EntryModuleProvider provider, ShopEntry shopEntry) {
		this.provider = provider;
		this.shopEntry = shopEntry;
	}

	public void setShopEntry(ShopEntry shopEntry) {
		this.shopEntry = shopEntry;
		if (this instanceof EntryModule module) {
			module.loadData();
		}
	}

	public Component getDisplayName() {
		return provider.getName();
	}

	public ItemStack getDisplayItem() {
		return provider.getListDisplayItem().clone();
	}
}
