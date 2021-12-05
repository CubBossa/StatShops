package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.shop.EntryTemplate;
import de.bossascrew.shops.shop.Shop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class TemplateHandler implements
		WebAccessable<EntryTemplate>,
		ListManagementMenuElementHolder<EntryTemplate> {

	@Getter
	private static TemplateHandler instance;

	Map<UUID, EntryTemplate> templateMap;

	public TemplateHandler() {
		instance = this;
		templateMap = ShopPlugin.getInstance().getDatabase().loadTemplates();
	}

	@Override
	public List<EntryTemplate> getValues() {
		return new ArrayList<>(templateMap.values());
	}

	public EntryTemplate createNew(String nameFormat, Shop shop, ShopMode shopMode, int page) {
		return null;
	}

	@Override
	public EntryTemplate createNew(String input) {
		return null;
	}

	@Override
	public EntryTemplate createDuplicate(EntryTemplate element) {
		return null;
	}

	@Override
	public boolean delete(EntryTemplate element) {
		return false;
	}

	@Override
	public List<EntryTemplate> getWebData() {
		return getValues();
	}

	@Override
	public void storeWebData(List<EntryTemplate> values) {
		//TODO
	}
}
