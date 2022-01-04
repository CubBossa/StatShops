package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.data.LogDatabase;

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

	public FlatFileLogDatabase(File directory, boolean filePerShop, boolean directoryPerShop, boolean filePerDay, boolean directoryPerDay) {
		if (!filePerShop) {
			shopPrefix = shop -> "shop: '" + shop.getUUID() + "', ";
		}
		fileProvider = shop -> {
			File dir = directoryPerShop ? new File(directory, shop.getUUID().toString()) : directory;
			dir.mkdir();
			dir = directoryPerDay ? new File(dir, LocalDate.now().toString()) : dir;
			dir.mkdir();

			String first = COMMON_FILE_NAME;
			String second = "";

			if (!directoryPerDay && filePerDay) {
				second = " - " + LocalDate.now();
			}
			if (!directoryPerShop && filePerShop) {
				first = shop.getUUID().toString();
			}
			File file = new File(dir, first + second + ".txt");
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
		if (entry == null) {
			return;
		}
		File file = fileProvider.apply(shop);
		try (BufferedWriter buffer = new BufferedWriter(new FileWriter(file, true))) {
			buffer.append(shopPrefix.apply(shop)).append(entry.getData());
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
