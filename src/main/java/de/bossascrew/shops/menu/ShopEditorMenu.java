package de.bossascrew.shops.menu;

import de.bossascrew.shops.shop.entry.ShopEntry;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.jetbrains.annotations.Nullable;

public class ShopEditorMenu extends BottomTopChestMenu {

	@Getter
	@Setter
	private @Nullable ShopEntry selectedEntry = null;

	public ShopEditorMenu(Component title, int rows, int bottomRows) {
		super(title, rows, bottomRows);
	}
}
