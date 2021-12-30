package de.bossascrew.shops.statshops.handler;

import lombok.Getter;

public class DatabaseHandler {

	@Getter
	private static DatabaseHandler instance;

	public DatabaseHandler() {
		instance = this;
	}

}
