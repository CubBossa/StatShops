package de.bossascrew.shops.general.util;

import lombok.Getter;

import java.util.logging.Level;

@Getter
public enum LoggingPolicy {

	DEBUG(Level.INFO, 0),
	INFO(Level.INFO, 1),
	WARN(Level.WARNING, 2),
	ERROR(Level.SEVERE, 3);

	private final Level level;
	private final int priotiry;

	LoggingPolicy(Level level, int priority) {
		this.level = level;
		this.priotiry = priority;
	}
}
