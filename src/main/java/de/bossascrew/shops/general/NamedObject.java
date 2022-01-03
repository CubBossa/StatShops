package de.bossascrew.shops.general;

import net.kyori.adventure.text.Component;

public interface NamedObject {


	/**
	 * @return The name as styled kyori component
	 */
	Component getName();

	/**
	 * @return The name format in minimessage style
	 */
	String getNameFormat();

	/**
	 * @return The bare text name without formats and colors
	 */
	String getNamePlain();

	/**
	 * Allows to change the name format. The default implementation of this interface likewise sets the name component and the plain text string.
	 *
	 * @param nameFormat The name format in minimessage style.
	 */
	void setNameFormat(String nameFormat);
}
