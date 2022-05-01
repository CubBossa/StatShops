package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.menu.VillagerShopEditor;
import de.bossascrew.shops.statshops.menu.VillagerShopMenu;
import de.cubbossa.guiframework.inventory.Menu;
import de.cubbossa.guiframework.inventory.implementations.VillagerMenu;
import lombok.Getter;
import lombok.Setter;

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
	public boolean open(Customer customer) {
		new VillagerShopMenu(this, customer).open(customer.getPlayer());
		return true;
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

	public Menu newEditorMenu() {
		return new VillagerShopEditor(this);
	}
}
