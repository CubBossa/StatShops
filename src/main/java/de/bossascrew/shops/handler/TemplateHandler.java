package de.bossascrew.shops.handler;

import de.bossascrew.shops.shop.Template;
import lombok.Getter;

public class TemplateHandler {

	@Getter
	private static TemplateHandler instance;

	public TemplateHandler() {
		instance = this;
	}

	public Template getDefaultTemplate() {
		return new Template();
	}

}
