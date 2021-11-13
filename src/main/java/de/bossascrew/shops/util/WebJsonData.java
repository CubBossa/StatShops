package de.bossascrew.shops.util;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.shop.Discount;
import de.bossascrew.shops.shop.Limit;
import de.bossascrew.shops.shop.Shop;

import java.util.ArrayList;
import java.util.List;

public class WebJsonData {
    private List<Shop> shops;
    private List<Discount> discounts;
    private List<Limit> limits;

    public WebJsonData() {
        this.shops = ShopPlugin.getInstance().getShopHandler().getShops();
        this.limits = new ArrayList<Limit>(ShopPlugin.getInstance().getLimitsHandler().getLimitMap().values());
        this.discounts = ShopPlugin.getInstance().getDiscountHandler().getDiscounts();
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
