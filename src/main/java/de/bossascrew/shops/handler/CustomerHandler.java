package de.bossascrew.shops.handler;

import de.bossascrew.shops.Customer;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.bukkit.entity.Player;

import java.util.*;

public class CustomerHandler implements WebAccessable<Customer> {

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

	public List<Customer> getCustomers() {
		return new ArrayList<>(customerMap.values());
	}

	@Override
	public List<Customer> getWebData() {
		return getCustomers();
	}

	@Override
	public void storeWebData(List<Customer> values) {
		//TODO
	}
}
