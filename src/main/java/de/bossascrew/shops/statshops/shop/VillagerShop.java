package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.general.menu.VillagerMenu;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.CloseContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.menu.VillagerShopEditor;
import de.bossascrew.shops.statshops.menu.VillagerShopMenu;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.Nullable;

import java.util.UUID;

@Getter
@Setter
public class VillagerShop extends BaseShop implements Shop {

	public VillagerShop(String nameFormat) {
		this(UUID.randomUUID(), nameFormat);
	}

	public VillagerShop(UUID uuid, String nameFormat) {
		super(uuid, nameFormat);
	}

	@Override
	public boolean open(Customer customer, @Nullable ContextConsumer<CloseContext> closeHandler) {
		return new VillagerShopMenu(this, customer).openInventory(customer.getPlayer()) != null;
	}

	public boolean close(Customer customer) {
		VillagerMenu menu = (VillagerMenu) menuMap.get(customer);
		if (menu != null) {
			if (customer.getPlayer().getOpenInventory().getTopInventory().equals(menu.getInventory())) {
				customer.getPlayer().closeInventory();
			}
			menuMap.remove(customer);
		}
		return activeCustomers.remove(customer);
	}

	@Override
	public void openEditorMenu(Player player, ContextConsumer<BackContext> backHandler) {
		new VillagerShopEditor(this, backHandler).openInventory(player);
	}
}
