package de.bossascrew.shops.statshops.shop;

import com.google.common.base.Preconditions;
import de.bossascrew.shops.statshops.api.data.NamedObject;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.general.menu.ListMenuElement;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.util.Duplicable;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.objecthunter.exp4j.ExpressionBuilder;
import org.bukkit.inventory.ItemStack;

import java.util.*;

public class EntryTemplate implements ListMenuElement, Duplicable<EntryTemplate>, NamedObject {

	@Getter
	private final UUID uuid;
	@Getter
	@Setter
	private short discIndex = 0;

	@Getter
	private String nameFormat;
	@Getter
	private String namePlain;
	@Getter
	private Component name;
	/**
	 * Shopentries
	 */
	private final List<Pair<ShopEntry, String>> entries;

	public EntryTemplate(UUID uuid, String nameFormat) {
		super();
		this.uuid = uuid;
		setNameFormat(nameFormat);
		this.entries = new ArrayList<>();
	}

	public void setNameFormat(String nameFormat) {
		this.nameFormat = nameFormat;
		this.name = StatShops.getInstance().getMiniMessage().parse(nameFormat);
		this.namePlain = TextUtils.toPlain(this.name);
	}

	@Override
	public ItemStack getListDisplayItem() {
		return ItemStackUtils.createTemplatesItemStack(this);
	}

	public ShopEntry put(Integer slot, ShopEntry entry) {
		Preconditions.checkNotNull(slot);
		Preconditions.checkArgument(slot >= 0 && slot < RowedOpenableMenu.LARGEST_INV_SIZE, "slot \"%d\" needs to be \"0 <= slot < 6 * 9\" ", slot);
		entries.add(new Pair<>(entry, "" + slot));
		return entry;
	}

	public ShopEntry put(String function, ShopEntry entry) {
		entries.add(new Pair<>(entry, function));
		return entry;
	}

	public List<Pair<ShopEntry, String>> getEntries() {
		return entries;
	}

	public Map<Integer, ShopEntry> getEntries(int rows) {
		Map<Integer, ShopEntry> mapped = new HashMap<>();
		for (Pair<ShopEntry, String> entry : entries) {
			int slot = (int) new ExpressionBuilder(entry.getRight().replace("<rows>", rows + "")
					.replace("<row>", rows + "")).build().evaluate();
			entry.getLeft().setSlot(slot);
			mapped.put(slot, entry.getLeft());
		}
		return mapped;
	}

	public int size() {
		return entries.size();
	}

	@Override
	public EntryTemplate duplicate() {
		EntryTemplate duplicate = new EntryTemplate(UUID.randomUUID(), nameFormat);
		for (Pair<ShopEntry, String> entry : entries) {
			duplicate.put(entry.getRight(), entry.getLeft().duplicate());
		}
		return duplicate;
	}
}
