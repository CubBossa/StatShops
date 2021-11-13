package de.bossascrew.shops.shop;

import lombok.Getter;

public enum ShopInteractionResult {
	FAIL_SHOP_DISABLED(10),
	FAIL_VAULT_UNKNOWN(10),
	FAIL_NO_ENTRY(8),
	FAIL_NO_PERMISSION(8),
	FAIL_CANT_AFFORD(6),
	FAIL_CANT_REWARD(4),
	SUCCESS(2);

	@Getter
	private final int priority;

	ShopInteractionResult(int priority) {
		this.priority = priority;
	}

	public ShopInteractionResult compare(ShopInteractionResult result) {
		return (priority > result.priority) ? this : result;
	}

}
