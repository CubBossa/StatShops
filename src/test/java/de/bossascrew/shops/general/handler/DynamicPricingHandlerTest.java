package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.statshops.handler.DynamicPricingHandler;
import org.junit.jupiter.api.Test;

import java.util.Map;

class DynamicPricingHandlerTest {

	@Test
	void getDefaultPrice() {
		DynamicPricingHandler dph = new DynamicPricingHandler();
		dph.loadDefaultPricing("test", key -> Map.of("test", 3.5));
		//assertEquals("asd3.5lol<>", dph.insertDefaultPrice("asd<db:test>lol<>"));
		//assertEquals("10.0lol<>", dph.insertDefaultPrice("<db:asd<db:test>lol<>"));
	}
}