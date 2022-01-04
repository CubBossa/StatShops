package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.api.data.Database;
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
	}

}
