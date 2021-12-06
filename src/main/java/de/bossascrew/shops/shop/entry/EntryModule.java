package de.bossascrew.shops.shop.entry;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.data.LogEntry;
import de.bossascrew.shops.data.Message;
import de.bossascrew.shops.shop.ShopInteractionResult;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

public interface EntryModule {

	Component getDisplayName();

	ItemStack getDisplayItem();

	DataSlot<?>[] getDataSlots();

	void loadData();

	void saveData();

	ShopInteractionResult perform(Customer customer);

	@Nullable LogEntry createLogEntry(Customer customer, ShopInteractionResult result);

	@RequiredArgsConstructor
	@Getter
	class DataSlot<T> {
		//TODO vorlagen mit Name type und lore extern erstellen und hier nur Instanzen

		private final Class<T> type;
		private final Message name;
		private final Message lore;
		@Setter
		private @Nullable T data;
	}
}
