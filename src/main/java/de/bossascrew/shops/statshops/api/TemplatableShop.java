package de.bossascrew.shops.statshops.api;

import de.bossascrew.shops.statshops.shop.EntryTemplate;
import org.jetbrains.annotations.Nullable;

public interface TemplatableShop extends Shop {

	void applyTemplate(EntryTemplate template);

	@Nullable EntryTemplate getDefaultTemplate();

	void setDefaultTemplate(@Nullable EntryTemplate defaultTemplate);
}
