package de.bossascrew.shops.statshops.events;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;
import org.jetbrains.annotations.NotNull;

public class SimpleEvent extends Event {

	private static final HandlerList handlers = new HandlerList();

	@NotNull
	@Override
	public HandlerList getHandlers() {
		return handlers;
	}
}
