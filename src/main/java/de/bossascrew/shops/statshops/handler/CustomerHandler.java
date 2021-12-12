package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.web.WebAccessable;
import lombok.Getter;
import org.bukkit.entity.Player;

import java.util.*;

public class CustomerHandler implements WebAccessable<Customer> {

	@Getter
	private static CustomerHandler instance;

	private final Map<UUID, Customer> customerMap;

	public CustomerHandler() {
		instance = this;
		customerMap = new HashMap<>();
	}

	public Customer getCustomer(Player player) {
		Customer customer = customerMap.get(player.getUniqueId());
		if (customer == null) {
			customer = StatShops.getInstance().getDatabase().loadCustomer(player.getUniqueId());
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
