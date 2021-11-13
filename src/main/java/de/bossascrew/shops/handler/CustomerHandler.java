package de.bossascrew.shops.handler;

import de.bossascrew.shops.Customer;
import lombok.Getter;
import org.bukkit.entity.Player;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class CustomerHandler {

	@Getter
	private static CustomerHandler instance;

	Map<UUID, Customer> customerMap;

	public CustomerHandler() {
		instance = this;
		customerMap = new HashMap<>();
	}

	public Customer getCustomer(Player player) { //TODO stattdessen datenbank kram
		Customer customer =  customerMap.get(player.getUniqueId());
		if(customer == null) {
			customer = new Customer(player, new HashMap<>(), new HashMap<>());
			customerMap.put(player.getUniqueId(), customer);
		}
		return customer;
	}
}
