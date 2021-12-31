package de.bossascrew.shops.statshops.data;

import lombok.Data;
import lombok.Getter;

@Data
public class LogEntry {

	private final String customerUuid;
	private final String shopEntryUuid;
	private final String interactionType;
	private final String payPrices;
	private final String gainPrices;
	private final String timeStamp;
	private final String discount;
	private final String accountedDiscounts;
	private final String accountedLimits;

	@Override
	public String toString() {
		return "customer='" + customerUuid + '\'' +
				", entry='" + shopEntryUuid + '\'' +
				", type='" + interactionType + '\'' +
				", timeStamp='" + timeStamp + '\'' +
				", discount='" + discount + '\'' +
				", payPrices='" + payPrices + '\'' +
				", gainPrices='" + gainPrices + '\'' +
				", accountedDiscounts='" + accountedDiscounts + '\'' +
				", accountedLimits='" + accountedLimits + '\'';
	}
}
