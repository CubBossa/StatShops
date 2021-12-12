package de.bossascrew.shops.general.menu.contexts;

import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

public class ActionContext<T> extends PlayerContext {

	@Getter
	private final ItemStack itemStack;
	@Getter
	private final int slot;
	@Getter
	private final T action;
	@Getter
	private boolean itemStackChanged;
	private ItemStack effectiveItemStack = null;
	@Getter
	@Setter
	private boolean cancelled;

	public ActionContext(Player player, ItemStack itemStack, int slot, T action) {
		super(player);
		this.itemStack = itemStack;
		this.slot = slot;
		this.action = action;
	}

	public ActionContext(Player player, ItemStack itemStack, int slot, T action, boolean cancelled) {
		super(player);
		this.itemStack = itemStack;
		this.slot = slot;
		this.action = action;
		this.cancelled = cancelled;
	}

	public void setItemStack(ItemStack itemStack) {
		this.effectiveItemStack = itemStack;
		itemStackChanged = true;
	}

	public ItemStack getEffectiveItemStack() {
		return effectiveItemStack == null ? itemStack : effectiveItemStack;
	}
}
