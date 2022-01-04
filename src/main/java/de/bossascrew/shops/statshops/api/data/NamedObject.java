package de.bossascrew.shops.statshops.api.data;

import net.kyori.adventure.text.Component;

/**
 * Ensures that the object has a component name that is generated from a format string.
 * The string can be formatted with the Kyori MiniMessage format.
 */
public interface NamedObject {

	/**
	 * @return The name as styled Kyori Adventure component, generated from the name format String.
	 */
	Component getName();

	/**
	 * @return The name format in Kyori MiniMessage format
	 */
	String getNameFormat();

	/**
	 * @return The bare text name without formats and colors, generated from the name format String.
	 */
	String getNamePlain();

	/**
	 * Allows to change the name format. Setting this value also updates the name component and the plain text string.
	 *
	 * @param nameFormat The name format in minimessage style.
	 */
	void setNameFormat(String nameFormat);
}
