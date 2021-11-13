package de.bossascrew.shops.util;

import lombok.Data;

@Data
public class Pair<L, R> {

	private final L left;
	private final R right;

	public static <L, R> Pair<L, R> of(L left, R right) {
		return new Pair<>(left, right);
	}
}
