package de.bossascrew.shops.general.handler;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.DefaultPricingDatabase;
import lombok.Getter;
import lombok.Setter;
import org.jetbrains.annotations.Nullable;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class DynamicPricingHandler {

	@Getter
	private static DynamicPricingHandler instance;


	private final ScriptEngine scriptEngine;
	/**
	 * A function that will be applied to every price that ever occurs in a shop as final step.
	 */
	@Getter
	@Setter
	private Function<Double, Double> dynamicPricingProcessor = aDouble -> aDouble;
	private final List<Template> defaultTemplates;

	private final Map<String, Double> defaultPricing;

	public DynamicPricingHandler() {
		instance = this;

		scriptEngine = new ScriptEngineManager().getEngineByName("Nashorn");
		defaultTemplates = new ArrayList<>();
		defaultPricing = new HashMap<>();
	}

	/**
	 * Allows dynamic pricing. Every trade module price will be processed by this method.
	 *
	 * @param priceInput A string that defines the price. E.g. {@code 3 * <db:diamond> / 10}
	 * @param templates  All templates that are supposed to be parsed before calculating the equation
	 * @return the result of the equation or null if it was not possible to parse as double
	 */
	public @Nullable Double getPrice(String priceInput, Template... templates) {
		if (!StatShops.getInstance().getShopsConfig().isDynamicPricingEnabled()) {
			try {
				return Double.parseDouble(priceInput);
			} catch (NumberFormatException e) {
				return 10.;
			}
		}

		List<Template> templateList = new ArrayList<>(defaultTemplates);
		templateList.addAll(List.of(templates));
		for (Template template : templateList) {
			priceInput = priceInput.replace(template.placeholder(), template.value() + "");
		}

		priceInput = insertDefaultPrice(priceInput);

		try {
			return dynamicPricingProcessor.apply((Double) scriptEngine.eval(priceInput));
		} catch (ScriptException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * @return a version of the same string, where the placeholder of format: {@Code <db:.*>} will be replaced by the default
	 * pricing from the default pricing table.
	 */
	private String insertDefaultPrice(String input) {
		String start = "<db:";
		int index = input.indexOf(start);
		int endIndex = index;
		while (endIndex < input.length() && input.charAt(endIndex) != '>') {
			endIndex++;
		}

		String key = input.substring(index + start.length(), endIndex);
		Double result = defaultPricing.get(key);
		if (result == null) {
			StatShops.getInstance().log(LoggingPolicy.WARN, "A shop with dynamic pricing requested the default pricing: '" + key + "', which does not exist." +
					"The plugin will automatically load 10.0 as default.");
		}
		return input.substring(0, index) + result + input.substring(endIndex + 1, input.length());
	}

	public void loadDefaultPricing(DefaultPricingDatabase database) {
		this.defaultPricing.putAll(database.loadPricing());
	}

	public Template registerTemplate(String placeholder, double value) {
		Template template = new Template(placeholder, value);
		defaultTemplates.add(template);
		return template;
	}

	public void unregisterTemplate(Template template) {
		defaultTemplates.remove(template);
	}

	public record Template(String placeholder, double value) {
	}
}
