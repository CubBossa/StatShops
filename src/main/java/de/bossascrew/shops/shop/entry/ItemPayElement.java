package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.ShopInteractionResult;
import org.bukkit.inventory.ItemStack;

public class ItemPayElement implements EntryElement {

	private final ItemStack itemStack;

	public ItemPayElement(ItemStack itemStack) {
		this.itemStack = itemStack;
	}

	@Override
	public String getAmountDisplay() {
		return itemStack.getAmount() + "";
	}

	public boolean canAct(Customer customer) {
		return customer.getPlayer().getInventory().containsAtLeast(itemStack, itemStack.getAmount());
	}

	public ShopInteractionResult act(Customer customer) {
		if (!canAct(customer)) {
			return ShopInteractionResult.FAIL_CANT_AFFORD;
		}
		customer.getPlayer().getInventory().removeItem(itemStack);
		return ShopInteractionResult.SUCCESS;
	}

	@Override
	public EntryElement duplicate() {
		return new ItemPayElement(this.itemStack.clone());
	}
}
