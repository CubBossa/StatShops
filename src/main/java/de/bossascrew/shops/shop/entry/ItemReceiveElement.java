package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.shop.ShopInteractionResult;
import de.bossascrew.shops.util.ItemStackUtils;
import lombok.Getter;
import org.bukkit.inventory.ItemStack;

public class ItemReceiveElement implements EntryElement {

	@Getter
	private final ItemStack itemStack;

	public ItemReceiveElement(ItemStack itemStack) {
		this.itemStack = itemStack;
	}

	@Override
	public String getAmountDisplay() {
		return itemStack.getAmount() + "";
	}

	@Override
	public boolean canAct(Customer customer) {
		return true;
	}

	@Override
	public ShopInteractionResult act(Customer customer) {
		ItemStackUtils.giveOrDrop(customer.getPlayer(), itemStack);
		return ShopInteractionResult.SUCCESS;
	}

	@Override
	public EntryElement duplicate() {
		return new ItemReceiveElement(itemStack.clone());
	}
}
