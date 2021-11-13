package de.bossascrew.shops.shop;

import java.util.List;

public interface Taggable {

	List<String> getTags();

	boolean addTag(String tag);

	boolean removeTag(String tag);

	boolean hasTag(String tag);
}
