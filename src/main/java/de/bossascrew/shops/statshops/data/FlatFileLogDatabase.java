package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.function.Function;

public class FlatFileLogDatabase implements LogDatabase {

	private static final String COMMON_FILE_NAME = "logs";
	private final Function<Shop, File> fileProvider;
	private Function<Shop, String> shopPrefix = shop -> "";

	public FlatFileLogDatabase(File directory, boolean filePerShop, boolean filePerDay) {
		if (!filePerShop) {
			shopPrefix = shop -> "shop: '" + shop.getUUID() + "', ";
		}
		fileProvider = shop -> {
			String fileName = filePerShop ? shop.getUUID().toString() : COMMON_FILE_NAME;
			fileName = filePerDay ? fileName + " - " + LocalDate.now() : "";
			File file = new File(directory, fileName + ".txt");
			if (!file.exists()) {
				try {
					file.createNewFile();
				} catch (IOException e) {
					StatShops.getInstance().log(LoggingPolicy.ERROR, "Error while creating log file", e);
				}
			}
			return file;
		};
	}

	@Override
	public void logToDatabase(LogEntry entry, Shop shop) {
		File file = fileProvider.apply(shop);
		try (BufferedWriter buffer = new BufferedWriter(new FileWriter(file, true))) {
			buffer.append(shopPrefix.apply(shop)).append(entry.toString());
			buffer.newLine();

		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Error while logging transaction", e);
		}
	}

	@Override
	public List<LogEntry> loadLogsFromDatabase(LocalDateTime since) {
		return null;
	}
}
