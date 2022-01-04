package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.api.data.DefaultPricingDatabase;
import lombok.Getter;
import lombok.Setter;
import net.objecthunter.exp4j.Expression;
import net.objecthunter.exp4j.ExpressionBuilder;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class DynamicPricingHandler {

	@Getter
	private static DynamicPricingHandler instance;

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

		defaultTemplates = new ArrayList<>();
		defaultPricing = new HashMap<>();

		registerExternalPricings();
	}

	private void registerExternalPricings() {
		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.registerDynamicPricingDefaultValues(this);
		}
	}

	/**
	 * Allows dynamic pricing. Every trade module price will be processed by this method.
	 *
	 * @param priceInput A string that defines the price. E.g. {@code 3 * <db:diamond> / 10}
	 * @param templates  All templates that are supposed to be parsed before calculating the equation
	 * @return the result of the equation or null if it was not possible to parse as double
	 */
	public @Nullable Double getPrice(Customer customer, String priceInput, Template... templates) {
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
			priceInput = priceInput.replace(template.placeholder(), template.value().apply(new DynamicPricingContext(customer)) + "");
		}

		while (priceInput.contains("<db:")) {
			priceInput = insertDefaultPrice(priceInput);
		}

		Expression e = new ExpressionBuilder(priceInput).build();
		return dynamicPricingProcessor.apply(e.evaluate());
	}

	/**
	 * @return a version of the same string, where the placeholder of format: {@Code <db:.*>} will be replaced by the default
	 * pricing from the default pricing table.
	 */
	private String insertDefaultPrice(String input) {
		String start = "<db:";
		int index = input.indexOf(start);
		int endIndex = index;
		if (index == -1) {
			return input;
		}
		while (endIndex < input.length() - 1 && input.charAt(endIndex) != '>') {
			endIndex++;
		}

		String key = input.substring(index + start.length(), endIndex);
		Double result = defaultPricing.get(key);
		if (result == null) {
			StatShops.getInstance().log(LoggingPolicy.WARN, "A shop with dynamic pricing requested the default pricing: '" + key + "', which does not exist." +
					"The plugin will automatically load 10.0 as default.");
		}
		return input.substring(0, index) + result + input.substring(endIndex + 1);
	}

	public void loadDefaultPricing(String key, DefaultPricingDatabase database) {
		Map<String, Double> map = database.loadPricing(key);
		StatShops.getInstance().log(LoggingPolicy.INFO, "Loaded default-value-database with " + map.size() + " entries.");
		this.defaultPricing.putAll(map);
	}

	public Template registerTemplate(StatShopsExtension extension, String placeholder, double value) {
		return registerTemplate(extension, placeholder, dynamicPricingContext -> value);
	}

	public Template registerTemplate(StatShopsExtension extension, String placeholder, Function<DynamicPricingContext, Double> value) {
		Template template = new Template(extension.getPlugin().getName() + ":" + placeholder, value);
		defaultTemplates.add(template);
		return template;
	}

	public void unregisterTemplate(Template template) {
		defaultTemplates.remove(template);
	}

	public record Template(String placeholder, Function<DynamicPricingContext, Double> value) {
	}

	public record DynamicPricingContext(@Nullable Customer customer) {
	}
}
