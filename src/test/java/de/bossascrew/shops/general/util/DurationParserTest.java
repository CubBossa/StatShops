package de.bossascrew.shops.general.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.temporal.ChronoUnit;

class DurationParserTest {

	@Test
	public void testFormat1() {
		Duration d = Duration.of(3, ChronoUnit.HOURS);
		Assertions.assertEquals("3h ", new DurationParser().format(d));
	}

	@Test
	public void testFormat2() {
		Duration d = Duration.of(7, ChronoUnit.DAYS).plus(Duration.of(3, ChronoUnit.HOURS));
		Assertions.assertEquals("1w 3h ", new DurationParser().format(d));
	}

	@Test
	public void testParse1() {
		Duration d = new DurationParser().parse("3d 17h 10min ");
		Assertions.assertEquals(d, Duration.ofDays(3).plus(17, ChronoUnit.HOURS).plus(10, ChronoUnit.MINUTES));
	}

	@Test
	public void testParse2() {
		Duration d = new DurationParser().parse("3d17h 11min");
		Assertions.assertEquals(d, Duration.ofDays(3).plus(17, ChronoUnit.HOURS).plus(11, ChronoUnit.MINUTES));
	}

}