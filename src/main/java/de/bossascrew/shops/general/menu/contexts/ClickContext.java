package de.bossascrew.shops.general.menu.contexts;

import lombok.Getter;
import lombok.Setter;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

@Getter
@Setter
public class ClickContext extends ActionContext<ClickType> {

	public ClickContext(Player player, ItemStack itemStack, int slot, ClickType action) {
		super(player, itemStack, slot, action);
	}

	public ClickContext(Player player, ItemStack itemStack, int slot, ClickType action, boolean cancelled) {
		super(player, itemStack, slot, action, cancelled);
	}
}
