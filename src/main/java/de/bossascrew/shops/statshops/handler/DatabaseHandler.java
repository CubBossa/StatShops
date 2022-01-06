package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.api.data.Database;
import de.bossascrew.shops.statshops.data.Config;
import lombok.Getter;
import lombok.Setter;

public class DatabaseHandler {

	@Getter
	private static DatabaseHandler instance;

	@Getter
	@Setter
	private Database database;

	public DatabaseHandler() {
		instance = this;

		loadFromConfig(StatShops.getInstance().getShopsConfig());

		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.provideDatabase(this);
		}
	}

	public void loadFromConfig(Config config) {
		//TODO
	}
}
