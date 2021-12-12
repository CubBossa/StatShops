package de.bossascrew.shops.general;

import java.util.List;

/**
 * Contains 'tags' aka Strings. Tags can be added, removed and listed and will be used to check if for example a
 * limit or discount applies to a shop entry.
 */
public interface Taggable {

	/**
	 * @return a List of all tags
	 */
	List<String> getTags();

	/**
	 * @param tag the tag to add
	 * @return true, if the tag was successfully added. false, if an error occurred or if the tag was already set.
	 */
	boolean addTag(String tag);

	/**
	 * @param tag the tag to remove
	 * @return true, if the tag existed and was successfully removed. false, if this tag was not set for this Taggable
	 */
	boolean removeTag(String tag);

	/**
	 * @param tag the tag to look up
	 * @return true, if the tag is set, false if not
	 */
	boolean hasTag(String tag);
}
