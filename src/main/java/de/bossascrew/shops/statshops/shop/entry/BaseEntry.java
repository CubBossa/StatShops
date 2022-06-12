package de.bossascrew.shops.statshops.shop.entry;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.module.EntryModule;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Config;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.events.ShopEntryInteractEvent;
import de.bossascrew.shops.statshops.events.ShopEntryInteractedEvent;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.cubbossa.translations.Message;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Supplier;

/**
 * A shop entry to be used for all itembased shops (not for chat shops or hologram shops e.g.)
 */
@Getter
@Setter
public class BaseEntry implements ShopEntry {

	private final UUID uuid;
	private Shop shop;
	private ItemStack displayItem;
	private @Nullable String permission;
	private String infoLoreFormat;
	private final List<String> tags;
	private @Nullable EntryModule module = null;
	private final Map<String, DataSlot<?>> dataSlots;
	private int slot;

	public BaseEntry(UUID uuid, Shop shop, ItemStack displayItem, @Nullable String permission, int slot) {
		this.uuid = uuid;
		this.shop = shop;
		this.displayItem = displayItem;
		this.permission = permission;
		this.tags = new ArrayList<>();
		this.dataSlots = new HashMap<>();

		this.slot = slot;
	}

	public void setModule(@Nullable EntryModule module) {
		this.module = module;
		if (this.module != null) {
			this.module.setShopEntry(this);
		}
	}

	@Override
	public UUID getUUID() {
		return uuid;
	}

	public boolean hasPermission(Customer customer) {
		return permission == null || customer.getPlayer().hasPermission(permission);
	}

	@Override
	public Map<String, DataSlot<?>> getData() {
		return dataSlots;
	}

	@Override
	public <T extends DataSlot<?>> T getData(Class<T> clazz, String key, Supplier<T> fallbackValue) {
		DataSlot<?> dataSlot = dataSlots.get(key);
		if (dataSlot == null) {
			dataSlot = fallbackValue.get();
			dataSlots.put(key, dataSlot);
		}
		Pair<Message, Message> messagePair = DataSlot.DATA_MESSAGE_MAP.get(key);
		if (messagePair != null && dataSlot.getName() == null) {
			dataSlot.setName(messagePair.getLeft());
		}
		if (messagePair != null && dataSlot.getLore() == null) {
			dataSlot.setLore(messagePair.getRight());
		}
		return (T) dataSlot;
	}

	public EntryInteractionResult interact(Customer customer, ShopMenu menu, EntryInteractionType interactionType) {
		ShopEntryInteractEvent entryInteractEvent = new ShopEntryInteractEvent(this, customer, interactionType);
		Bukkit.getPluginManager().callEvent(entryInteractEvent);
		if (entryInteractEvent.isCancelled()) {
			return EntryInteractionResult.FAIL_UNKNOWN;
		}
		if (module == null) {
			return EntryInteractionResult.STATIC;
		}
		if (!hasPermission(customer)) {
			return EntryInteractionResult.FAIL_NO_PERMISSION;
		}
		EntryInteractionResult result = module.perform(customer, menu, interactionType);
		StatShops.getInstance().getLogDatabase().logToDatabase(module.createLogEntry(customer, result), shop);

		ShopEntryInteractedEvent event = new ShopEntryInteractedEvent(this, customer, interactionType, result);
		Bukkit.getPluginManager().callEvent(event);

		return result;
	}

	public List<String> getTags(boolean generated) {
		return generated ? getTags() : new ArrayList<>(tags);
	}

	public List<String> getTags() {
		// Don't add shop tags as they will always be processed twice in TagUtils
		List<String> tags = new ArrayList<>();
		tags.add(uuid.toString());
		tags.addAll(this.tags);
		//If auto-tagging is enabled, add all material tags to the entry
		if (module != null && module instanceof TradeModule tm) {
			Config config = StatShops.getInstance().getShopsConfig();

			if(config.isAutoTaggingArticle()) {
				tags.add("article:" + tm.getArticle().getProvider().getKey());
			}
			if(config.isAutoTaggingCurrency()) {
				tags.add("currency:" + tm.getCosts().getBuyPrice().getCurrency().getKey());
			}


			if (tm.getGainPrice().getObject() instanceof ItemStack soldStack) {
				tags.addAll(TagUtils.getTags(soldStack,
						config.isAutoTaggingMaterials(),
						config.isAutoTaggingGroups(),
						config.isAutoTaggingEnchantments(),
						config.isAutoTaggingPotions(),
						config.isAutoTaggingAttributes()));
			}
		}
		return tags;
	}

	@Override
	public boolean addTag(String tag) {
		return tags.add(tag);
	}

	@Override
	public boolean removeTag(String tag) {
		return tags.remove(tag);
	}

	@Override
	public boolean hasTag(String tag) {
		return getTags().contains(tag);
	}

	@Override
	public void saveToDatabase() {
		StatShops.getInstance().getDatabase().saveEntry(this);
	}

	@Override
	public ShopEntry duplicate() {
		BaseEntry entry = new BaseEntry(UUID.randomUUID(), shop, displayItem.clone(), permission, slot);
		if (module != null) {
			EntryModule m = module.duplicate();
			entry.setModule(m);
		}
		return entry;
	}

	@Override
	public @Nullable Player getEditor() {
		return shop.getEditor();
	}

	@Override
	public void setEditor(@Nullable Player editor) {
		shop.setEditor(editor);
	}
}
