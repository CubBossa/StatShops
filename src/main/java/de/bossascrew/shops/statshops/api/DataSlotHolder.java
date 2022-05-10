package de.bossascrew.shops.statshops.api;

import de.bossascrew.shops.statshops.shop.DataSlot;

import java.util.Map;
import java.util.function.Supplier;

public interface DataSlotHolder {

    void loadData(Map<String, DataSlot<?>> data);

    Map<String, DataSlot<?>> getData();

    <T extends DataSlot<?>> T getData(Class<T> clazz, String key, Supplier<T> fallbackValue);
}
