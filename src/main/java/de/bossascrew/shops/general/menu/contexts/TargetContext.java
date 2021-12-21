package de.bossascrew.shops.general.menu.contexts;

import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

@Getter
@Setter
public class TargetContext<A, T> extends ActionContext<A> {

	private final T target;

	public TargetContext(Player player, ItemStack itemStack, int slot, A action, T target) {
		this(player, itemStack, slot, true, action, target);
	}

	public TargetContext(Player player, ItemStack itemStack, int slot, boolean cancelled, A action, T target) {
		super(player, itemStack, slot, action, cancelled);
		this.target = target;
	}

	public TargetContext(ActionContext<A> clickContext, T target) {
		super(clickContext.getPlayer(), clickContext.getItemStack(), clickContext.getSlot(), clickContext.getAction(), clickContext.isCancelled());
		this.target = target;
	}
}