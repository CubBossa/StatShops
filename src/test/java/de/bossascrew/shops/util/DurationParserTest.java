package de.bossascrew.shops.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.temporal.ChronoUnit;

class DurationParserTest {

	@Test
	public void testParse1() {
		Duration d = Duration.of(3, ChronoUnit.HOURS);
		Assertions.assertEquals("3h ", new DurationParser().parse(d));
	}

	@Test
	public void testParse2() {
		Duration d = Duration.of(7, ChronoUnit.DAYS).plus(Duration.of(3, ChronoUnit.HOURS));
		Assertions.assertEquals("1w 3h ", new DurationParser().parse(d));
	}

}