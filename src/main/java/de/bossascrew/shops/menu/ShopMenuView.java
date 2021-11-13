package de.bossascrew.shops.menu;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.handler.DiscountHandler;
import de.bossascrew.shops.shop.ChestMenuShop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.Template;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * One object per player with opened shop menu. Don't keep but destroy when shop closed.
 */
@Getter
public class ShopMenuView {

	private final ChestMenuShop shop;
	private int activePage;
	private ShopMode activeShopMode;
	private Inventory inventory;
	private InventoryView inventoryView = null;

	public ShopMenuView(ChestMenuShop shop) {
		this.shop = shop;
	}

	private Inventory createInventory() {
		this.inventory = Bukkit.createInventory(null, shop.getRows(), ComponentUtils.toLegacy(
				Message.SHOP_GUI_TITLE.getTranslation(
						Template.of("name", shop.getName()),
						Template.of("page", "" + activePage),
						Template.of("mode", activeShopMode.getDisplayName()))
		));
		return inventory;
	}

	private void fillInventory() {
		Map<Integer, ShopEntry> entries = shop.getModeEntryMap().get(activeShopMode);
		int pageSlots = shop.getRows() * RowedOpenableMenu.ROW_SIZE;
		for (int i = pageSlots * activePage; i < pageSlots * (activePage + 1); i++) {
			if (!entries.containsKey(i)) {
				continue;
			}
			ShopEntry entry = entries.get(i);
			DiscountHandler.getInstance().subscribeToDisplayUpdates(this, entry);
			update(entry);
		}
	}

	public void update(ShopEntry entry) {
		ItemStack itemStack = entry.getDisplayItem().clone();
		List<Component> additionalLore = new ArrayList<>();
		DiscountHandler.getInstance().addDiscountsLore(this, entry, additionalLore);
		ItemStackUtils.addLore(itemStack, additionalLore);
		inventory.setItem(entry.getSlot(), itemStack);
	}

	public void openShop(Customer customer) {
		int page = shop.getPreferredOpenPage(customer);
		ShopMode shopMode = shop.getPreferredShopMode(customer);
		openShop(customer.getPlayer(), page, shopMode);
	}

	public void openShop(Player player, int page, ShopMode shopMode) {
		this.activePage = page;
		this.activeShopMode = shopMode;
		createInventory();
		fillInventory();
		this.inventoryView = player.openInventory(this.inventory);
	}
}
