package de.bossascrew.shops.statshops.shop;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.menu.VillagerShopEditor;
import de.bossascrew.shops.statshops.menu.VillagerShopMenu;
import de.cubbossa.menuframework.inventory.Menu;
import de.cubbossa.menuframework.inventory.TopMenu;
import de.cubbossa.menuframework.inventory.implementations.VillagerMenu;
import lombok.Getter;
import lombok.Setter;

import java.util.UUID;

@Getter
@Setter
public class VillagerShop extends BaseShop {

    public VillagerShop(String nameFormat) {
        this(UUID.randomUUID(), nameFormat);
    }

    public VillagerShop(UUID uuid, String nameFormat) {
        super(uuid, nameFormat);
    }

    @Override
    public Menu newShopMenu(Customer customer) {
        return new VillagerShopMenu(this, customer);
    }

    public TopMenu newEditorMenu() {
        return new VillagerShopEditor(this);
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
}
