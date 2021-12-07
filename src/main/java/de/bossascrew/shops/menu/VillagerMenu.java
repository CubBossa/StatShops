package de.bossascrew.shops.menu;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.handler.InventoryHandler;
import de.bossascrew.shops.menu.contexts.ClickContext;
import de.bossascrew.shops.menu.contexts.CloseContext;
import de.bossascrew.shops.menu.contexts.ContextConsumer;
import de.bossascrew.shops.menu.contexts.TargetContext;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.ComponentUtils;
import de.bossascrew.shops.util.LoggingPolicy;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.TreeMap;
import java.util.function.Consumer;

public class VillagerMenu extends OpenableMenu {

	@Getter
	private @Nullable Merchant merchant = null;
	private final Map<Integer, MerchantRecipe> offers;

	@Setter
	@Getter
	private @Nullable ContextConsumer<ClickContext> tradeSelectHandler = null;

	public VillagerMenu(Component title, @Nullable ContextConsumer<CloseContext> closeHandler) {
		super(title, InventoryType.MERCHANT, new int[]{0, 1, 2}, 0, null, closeHandler);
		setDefaultCancelled(false);

		this.offers = new TreeMap<>();
	}

	public void setTradeHandler(ContextConsumer<TargetContext<ClickType, Integer>> tradeHandler) {
		setClickHandler(2, clickContext -> tradeHandler.accept(new TargetContext<>(clickContext.getPlayer(), clickContext.getItemStack(),
				clickContext.getSlot(), clickContext.getAction(), inventory == null ? 0 : ((MerchantInventory) inventory).getSelectedRecipeIndex())));
	}

	public void setMerchantOffer(int slot, ItemStack costs, ItemStack article) {
		MerchantRecipe recipe = new MerchantRecipe(article, Integer.MAX_VALUE);
		recipe.addIngredient(costs);
		setMerchantOffer(slot, recipe);
	}

	public void setMerchantOffer(int slot, ItemStack costA, ItemStack costB, ItemStack article) {
		MerchantRecipe recipe = new MerchantRecipe(article, Integer.MAX_VALUE);
		recipe.addIngredient(costA);
		recipe.addIngredient(costB);
		setMerchantOffer(slot, recipe);
	}

	public void setMerchantOffer(int slot, MerchantRecipe recipe) {
		if (recipe.getIngredients().isEmpty()) {
			ShopPlugin.getInstance().log(LoggingPolicy.WARN, "A MerchantRecipe was set without any ingredients. Using fallback ingredient: 1 Emerald");
			recipe.addIngredient(new ItemStack(Material.EMERALD));
		}
		offers.put(slot, recipe);
	}

	public void handleTradeSelect(Player player, int slot) {
		if (tradeSelectHandler == null) {
			return;
		}
		try {
			tradeSelectHandler.accept(new ClickContext(player, null, slot, ClickType.LEFT));
		} catch (Throwable t) {
			ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "An error occured while handling trade selection.", t);
		}
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		merchant = Bukkit.createMerchant(ComponentUtils.toLegacy(title));
		merchant.setRecipes(offers.values().stream().toList());

		InventoryView view = player.openMerchant(merchant, true);
		if (view == null) {
			return null;
		}
		inventory = view.getTopInventory();

		for (int slot : slots) {
			ItemStack specialItem = specialItems.getOrDefault(slot, null);
			if (specialItem == null) {
				continue;
			}
			inventory.setItem(slot, specialItem.clone());
		}

		if (backHandler != null) {
			inventory.setItem(backSlot, DefaultSpecialItem.BACK.createSpecialItem());
		}

		if (inventoryPreparer != null) {
			try {
				inventoryPreparer.accept(inventory);
			} catch (Exception exc) {
				ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Fehler bei openInventorySync() von Spieler " + player.getName(), exc);
			}
		}

		InventoryHandler.getInstance().handleMenuOpen(player, this);
		openInventories.put(player.getUniqueId(), inventory);
		return view;
	}
}
