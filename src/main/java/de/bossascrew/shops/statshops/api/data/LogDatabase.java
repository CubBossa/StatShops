package de.bossascrew.shops.statshops.api.data;

import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.data.LogEntry;

import java.time.LocalDateTime;
import java.util.List;

public interface LogDatabase {

	void logToDatabase(LogEntry entry, Shop shop);

	List<LogEntry> loadLogsFromDatabase(LocalDateTime since);
}
