package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.Shop;

import java.time.LocalDateTime;
import java.util.List;

public interface LogDatabase {

	void logToDatabase(LogEntry entry, Shop shop);

	List<LogEntry> loadLogsFromDatabase(LocalDateTime since);
}
