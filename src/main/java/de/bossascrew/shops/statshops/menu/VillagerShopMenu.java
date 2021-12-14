package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.entry.TradeModule;
import de.bossascrew.shops.general.menu.ShopMenu;
import de.bossascrew.shops.general.menu.VillagerMenu;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.shop.ShopInteractionResult;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import de.bossascrew.shops.statshops.shop.currency.Price;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.MerchantRecipe;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.function.Consumer;

public class VillagerShopMenu extends VillagerMenu implements ShopMenu {

	private final static ClickType[] IGNORED = {ClickType.UNKNOWN, ClickType.WINDOW_BORDER_LEFT, ClickType.WINDOW_BORDER_RIGHT};

	private final VillagerShop villagerShop;
	private final Map<ShopEntry, Integer> recipeMap;
	private final Map<Integer, ShopEntry> entryMap;

	public VillagerShopMenu(VillagerShop villagerShop) {
		super(villagerShop.getName(), null);
		this.villagerShop = villagerShop;
		this.recipeMap = new HashMap<>();
		this.entryMap = new HashMap<>();

		setTradeHandler(targetContext -> {
			if (Arrays.stream(IGNORED).anyMatch(clickType -> targetContext.getAction().equals(clickType))) {
				return;
			}
			ShopEntry entry = entryMap.get(targetContext.getTarget());
			if(entry.getModule() != null) {
				StatShops.getInstance().getLogDatabase().logToDatabase(entry.getModule().createLogEntry(Customer.wrap(targetContext.getPlayer()), ShopInteractionResult.SUCCESS));
			}
		});
	}

	private void prepareInventory(Player player) {
		int i = 0;
		for (Map.Entry<Integer, ShopEntry> entry : villagerShop.getSlotEntryMap().entrySet()) {

			ShopEntry e = entry.getValue();
			//Only works with Currency<ItemStack> for now
			if (e.getModule() != null && e.getModule() instanceof TradeModule tm && tm.getPayPrice(true).getObject() instanceof ItemStack) {

				Price<ItemStack> payPrice = tm.getPayPrice(true);
				ItemStack price = payPrice.getObject();
				price.setAmount(Integer.min((int) payPrice.getAmount(), 127));

				MerchantRecipe recipe = new MerchantRecipe((ItemStack) tm.getGainPrice().getObject(), e.getPermission() == null || player.hasPermission(e.getPermission()) ? Integer.MAX_VALUE : 0);
				recipeMap.put(e, i);
				entryMap.put(i++, e);
				recipe.addIngredient(price);
				placeEntry(e);

				DiscountHandler.getInstance().subscribeToDisplayUpdates(this, e);
				LimitsHandler.getInstance().subscribeToDisplayUpdates(this, e);

				setMerchantOffer(entry.getKey(), recipe);
			}
		}
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		prepareInventory(player);
		return super.openInventorySync(player, inventoryPreparer);
	}

	@Override
	public boolean closeInventory(Player player) {
		DiscountHandler.getInstance().unsubscribeToDisplayUpdates(this);
		return super.closeInventory(player);
	}

	public void placeEntry(ShopEntry entry) {
		int index = recipeMap.get(entry);
		if (super.getMerchant() == null) {
			return;
		}
		MerchantRecipe recipe = super.getMerchant().getRecipe(index);
		//Limits
		recipe.setUses(0); //TODO limits
		recipe.setMaxUses(Integer.min(recipe.getMaxUses(), Integer.MAX_VALUE)); //TODO limits

		//Discounts
		double discount = DiscountHandler.getInstance().combineDiscounts(entry, entry.getShop());
		recipe.setPriceMultiplier((float) discount);
		//TODO debug
		recipe.setMaxUses(discount < 1 ? 0 : 1);

		setMerchantOffer(entry.getSlot(), recipe);
	}

	@Override
	public void updateEntry(ShopEntry entry) {
		placeEntry(entry);
		for (UUID uuid : openInventories.keySet()) {
			Player player = Bukkit.getPlayer(uuid);
			if (player != null) {
				openInventorySync(player, null);
			}
		}
	}
}
