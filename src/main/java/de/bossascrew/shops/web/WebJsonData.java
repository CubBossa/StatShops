package de.bossascrew.shops.web;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.general.Shop;

import java.util.ArrayList;
import java.util.List;

public class WebJsonData {
    private final List<Shop> shops;
    private final List<Discount> discounts;
    private final List<Limit> limits;

    public WebJsonData() {
        this.shops = StatShops.getInstance().getShopHandler().getShops();
        this.limits = new ArrayList<Limit>(StatShops.getInstance().getLimitsHandler().getLimitMap().values());
        this.discounts = StatShops.getInstance().getDiscountHandler().getDiscounts();
    }

    public List<Shop> getShops() {
        return shops;
    }

    public List<Discount> getDiscounts() {
        return discounts;
    }

    public List<Limit> getLimits() {
        return limits;
    }
}
