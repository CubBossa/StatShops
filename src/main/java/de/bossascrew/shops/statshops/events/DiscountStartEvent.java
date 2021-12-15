package de.bossascrew.shops.statshops.events;

import de.bossascrew.shops.statshops.shop.Discount;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.event.Cancellable;

import java.time.LocalDateTime;

@Getter
@Setter
public class DiscountStartEvent extends DiscountEvent implements Cancellable {

	private boolean cancelled = false;
	private LocalDateTime startDate;

	public DiscountStartEvent(Discount discount, LocalDateTime startDate) {
		super(discount);
		this.startDate = startDate;
	}
}
