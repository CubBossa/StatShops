package de.bossascrew.shops.handler;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.menu.DefaultSpecialItem;
import de.bossascrew.shops.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.menu.RowedOpenableMenu;
import de.bossascrew.shops.shop.EntryTemplate;
import de.bossascrew.shops.shop.PaginatedModedShop;
import de.bossascrew.shops.shop.ShopMode;
import de.bossascrew.shops.shop.entry.BaseEntry;
import de.bossascrew.shops.shop.entry.ShopEntry;
import de.bossascrew.shops.util.LoggingPolicy;
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

	private final Map<UUID, EntryTemplate> templateMap;

	public TemplateHandler() {
		instance = this;
		templateMap = ShopPlugin.getInstance().getDatabase().loadTemplates();
	}

	@Override
	public List<EntryTemplate> getValues() {
		return new ArrayList<>(templateMap.values());
	}

	public EntryTemplate createNew(String nameFormat, PaginatedModedShop shop, ShopMode shopMode, int page) {
		EntryTemplate template = createNew(nameFormat);
		for (ShopEntry entry : shop.getEntries(shopMode, page)) {
			template.put(entry.getSlot() % RowedOpenableMenu.LARGEST_INV_SIZE, entry.duplicate());
		}
		return template;
	}

	@Override
	public EntryTemplate createNew(String input) {
		EntryTemplate template = new EntryTemplate(UUID.randomUUID(), input);
		templateMap.put(template.getUuid(), template);
		return template;
	}

	@Override
	public EntryTemplate createDuplicate(EntryTemplate element) {
		return null;
	}

	@Override
	public boolean delete(EntryTemplate element) {
		return false;
	}

	public void registerTemplate(EntryTemplate template) {
		if (templateMap.containsKey(template.getUuid())) {
			ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Cannot register template. A template with this UUID was already registered.");
			return;
		}
		templateMap.put(template.getUuid(), template);
		ShopPlugin.getInstance().log(LoggingPolicy.INFO, "Template successfully registered: " + template.getUuid());
	}

	@Override
	public List<EntryTemplate> getWebData() {
		return getValues();
	}

	@Override
	public void storeWebData(List<EntryTemplate> values) {
		//TODO
	}


	public void registerDefaults() {

		//Only gray empty bottom line
		EntryTemplate bottomLine = new EntryTemplate(UUID.randomUUID(), "<white>Bottom Line");
		for (int i = 0; i < 9; i++) {
			int _i = i;
			bottomLine.put(row -> (row - 1) * 9 + _i, new BaseEntry(UUID.randomUUID(), null,
					DefaultSpecialItem.EMPTY_DARK.createSpecialItem(), null, i, null));
		}
		registerTemplate(bottomLine);

		//Only empty gray and next page button
		EntryTemplate nextPage = bottomLine.duplicate();
		nextPage.setNameFormat("<white>Bottom Line - Next Page");
		BaseEntry entry = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.NEXT_PAGE.createSpecialItem(), null,
				1, null);
		entry.setModule(EntryModuleHandler.openNextPage(entry, 1));
		nextPage.put(row -> (row - 1) * 9 + 1, entry);
		registerTemplate(nextPage);

	}
}
