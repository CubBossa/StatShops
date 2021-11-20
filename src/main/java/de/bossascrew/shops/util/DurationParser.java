package de.bossascrew.shops.util;

import lombok.AllArgsConstructor;
import org.jetbrains.annotations.Nullable;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;

public class DurationParser {

	private static final DurationUnit[] units = {
			new DurationUnit(ChronoUnit.YEARS, 31_536_000_000L, "y ", "y "),
			new DurationUnit(ChronoUnit.WEEKS, 604_800_000, "w ", "w "),
			new DurationUnit(ChronoUnit.DAYS, 86_400_000, "d ", "d "),
			new DurationUnit(ChronoUnit.HOURS, 3_600_000, "h ", "h "),
			new DurationUnit(ChronoUnit.MINUTES, 60_000, "min ", "min "),
			new DurationUnit(ChronoUnit.SECONDS, 1000, "s ", "s ")
	};

	private final boolean displayEmptyUnits;
	private final @Nullable List<ChronoUnit> displayedUnits;

	public DurationParser() {
		this(false);
	}

	public DurationParser(ChronoUnit... displayedUnits) {
		this(false, displayedUnits);
	}

	public DurationParser(boolean displayEmptyUnits, ChronoUnit... displayedUnits) {
		this.displayEmptyUnits = displayEmptyUnits;
		this.displayedUnits = new ArrayList<>(List.of(displayedUnits));
	}

	public String parse(Duration duration) {
		return parse(duration.toMillis());
	}

	public String parse(long millis) {
		StringBuilder result = new StringBuilder();
		for (DurationUnit unit : units) {
			if (displayedUnits != null && !displayedUnits.contains(unit.unit)) {
				continue;
			}
			int counter = 0;
			while (millis >= unit.milliseconds) {
				millis -= unit.milliseconds;
				counter++;
			}
			if (counter == 0) {
				if (this.displayEmptyUnits) {
					result.append(0).append(unit.plural);
				}
			} else if (counter == 1) {
				result.append(1).append(unit.singular);
			} else {
				result.append(counter).append(unit.plural);
			}
		}
		return result.toString();
	}

	@AllArgsConstructor
	private static class DurationUnit {
		ChronoUnit unit;
		long milliseconds;
		String singular;
		String plural;
	}

}
