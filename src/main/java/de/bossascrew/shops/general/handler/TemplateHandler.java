package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.general.PaginatedModedShop;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.general.menu.ListManagementMenuElementHolder;
import de.bossascrew.shops.general.menu.RowedOpenableMenu;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.Config;
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

	public static final UUID UUID_BOTTOM = UUID.fromString("00000000-0000-0000-0000-000000000001");
	public static final UUID UUID_BOTTOM_PREV = UUID.fromString("00000000-0000-0000-0000-000000000002");
	public static final UUID UUID_BOTTOM_NEXT = UUID.fromString("00000000-0000-0000-0000-000000000003");
	public static final UUID UUID_BOTTOM_PREV_NEXT = UUID.fromString("00000000-0000-0000-0000-000000000004");

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

	public EntryTemplate loadDefaultTemplateFromConfig(Config config) {
		try {
			UUID uuid = UUID.fromString(config.getDefaultTemplate());
			defaultTemplate = templateMap.get(uuid);
			return defaultTemplate;
		} catch (Exception e) {
			return null;
		}
	}

	public void registerDefaults() {

		registerTemplate(createGrayBaseLineEntry(UUID_BOTTOM, "<white>Bottom Line", false, false));
		registerTemplate(createGrayBaseLineEntry(UUID_BOTTOM_NEXT, "<white>Bottom Line - Next Page Only", true, false));
		registerTemplate(createGrayBaseLineEntry(UUID_BOTTOM_PREV, "<white>Bottom Line - Previous Page Only", false, true));
		registerTemplate(createGrayBaseLineEntry(UUID_BOTTOM_PREV_NEXT, "<white>Bottom Line - Paginated", true, true));
	}

	private EntryTemplate createGrayBaseLineEntry(UUID uuid, String nameFormat, boolean nextPage, boolean prevPage) {
		EntryTemplate bottomLine = new EntryTemplate(uuid, nameFormat);
		for (int i = 0; i < 9; i++) {
			int _i = i;
			bottomLine.put(row -> (row - 1) * 9 + _i, new BaseEntry(UUID.randomUUID(), null,
					DefaultSpecialItem.EMPTY_DARK.createSpecialItem(), null, i, null));
		}
		if(prevPage) {
			BaseEntry entryPrev1 = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.PREV_PAGE.createSpecialItem(), null,
					0, null);
			entryPrev1.setModule(EntryModuleHandler.openPrevPage(entryPrev1, 1));
			bottomLine.put(row -> (row - 1) * 9, entryPrev1);
		}
		if(nextPage) {
			BaseEntry entryNext1 = new BaseEntry(UUID.randomUUID(), null, DefaultSpecialItem.NEXT_PAGE.createSpecialItem(), null,
					1, null);
			entryNext1.setModule(EntryModuleHandler.openNextPage(entryNext1, 1));
			bottomLine.put(row -> (row - 1) * 9 + 1, entryNext1);
		}
		return bottomLine;
	}
}
