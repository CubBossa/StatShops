package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.general.PaginatedModedShop;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.ShopMode;
import de.bossascrew.shops.statshops.shop.entry.BaseEntry;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import lombok.Setter;

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

	@Getter
	@Setter
	private EntryTemplate defaultTemplate = null;

	public TemplateHandler() {
		instance = this;
		templateMap = StatShops.getInstance().getDatabase().loadTemplates();
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
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Cannot register template. A template with this UUID was already registered.");
			return;
		}
		templateMap.put(template.getUuid(), template);
		StatShops.getInstance().log(LoggingPolicy.INFO, "Template successfully registered: " + template.getUuid());
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
		nextPage.setNameFormat("<white>Bottom Line - Next Page Only");
		BaseEntry entryNext = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.NEXT_PAGE.createSpecialItem(), null,
				1, null);
		entryNext.setModule(EntryModuleHandler.openNextPage(entryNext, 1));
		nextPage.put(row -> (row - 1) * 9 + 1, entryNext);
		registerTemplate(nextPage);

		//Only empty gray and prev page button
		EntryTemplate prevPage = bottomLine.duplicate();
		prevPage.setNameFormat("<white>Bottom Line - Previous Page Only");
		BaseEntry entryPrev = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.PREV_PAGE.createSpecialItem(), null,
				0, null);
		entryPrev.setModule(EntryModuleHandler.openNextPage(entryPrev, 1));
		prevPage.put(row -> (row - 1) * 9, entryPrev);
		registerTemplate(prevPage);

		//Only empty gray and prev page button
		EntryTemplate paginated = bottomLine.duplicate();
		paginated.setNameFormat("<white>Bottom Line - Paginated");
		BaseEntry entryPrev1 = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.PREV_PAGE.createSpecialItem(), null,
				0, null);
		entryPrev.setModule(EntryModuleHandler.openNextPage(entryPrev1, 1));
		BaseEntry entryNext1 = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.NEXT_PAGE.createSpecialItem(), null,
				1, null);
		entryPrev.setModule(EntryModuleHandler.openNextPage(entryNext1, 1));
		paginated.put(row -> (row - 1) * 9, entryPrev1);
		paginated.put(row -> (row - 1) * 9 + 1, entryNext1);
		registerTemplate(paginated);
		defaultTemplate = paginated;
	}
}
