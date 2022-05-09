package de.bossascrew.shops.statshops.menu;

import com.google.common.collect.Lists;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.handler.CurrencyHandler;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.shop.*;
import de.bossascrew.shops.statshops.shop.currency.Price;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.statshops.util.TradeMessageType;
import de.cubbossa.menuframework.inventory.Button;
import de.cubbossa.menuframework.inventory.implementations.VillagerMenu;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.MerchantRecipe;

import java.time.LocalDateTime;
import java.util.*;

public class VillagerShopMenu extends VillagerMenu implements ShopMenu {

    private final static ClickType[] IGNORED = {ClickType.UNKNOWN, ClickType.WINDOW_BORDER_LEFT, ClickType.WINDOW_BORDER_RIGHT};

    private final Shop villagerShop;
    private final Customer targetCustomer;
    private final SimpleBalanceMessenger balanceMessenger;
    private final Map<ShopEntry, TradeButton> trades;

    public VillagerShopMenu(Shop villagerShop, Customer customer) {
        super(villagerShop.getName());
        this.villagerShop = villagerShop;
        this.balanceMessenger = new SimpleBalanceMessenger(StatShops.getInstance().getShopsConfig().getTradeMessageFeedback());
        this.targetCustomer = customer;
        this.trades = new HashMap<>();
    }

    private void prepareInventory(Player player) {

        clearContent();

        for (ShopEntry e : villagerShop.getEntries().values()) {
            if(e == null) {
                continue;
            }

            //Only works with Currency<ItemStack> for now
            if (e.getModule() instanceof TradeModule tm && tm.getCosts().getBuyPrice().getCurrency().equals(CurrencyHandler.CURRENCY_ITEM)) {

                Price<ItemStack> payPrice = (Price<ItemStack>) tm.getPayPrice(true);
                ItemStack price = payPrice.getObject();
                price.setAmount(Integer.min((int) payPrice.getAmount(), 127));

                MerchantRecipe recipe = MerchantBuilder.builder()
                        .withArticle((ItemStack) tm.getGainPrice().getObject())
                        .withLeftCost(price)
                        .withUses(0, e.getPermission() == null || player.hasPermission(e.getPermission()) ? Integer.MAX_VALUE : 0)
                        .build();

                System.out.println("Adding recipe with " + recipe.getResult().getType());

                DiscountHandler.getInstance().subscribeToDisplayUpdates(this, e);
                LimitsHandler.getInstance().subscribeToDisplayUpdates(this, player, e);

                TradeButton button = addMerchantOffer(recipe, Button.builder().withClickHandler(VillagerMenu.ATTEMPT_BUY, c -> {
                    Customer customer = Customer.wrap(c.getPlayer());
                    if (Arrays.stream(IGNORED).anyMatch(clickType -> c.getAction().equals(clickType))) {
                        return;
                    }
                    if (StatShops.getInstance().getShopsConfig().getTradeMessageFeedback() != TradeMessageType.NONE) {
                        TradeModule tradeModule = (TradeModule) e.getModule();

                        List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(customer.getPlayer(), e, e.getShop());
                        double discount = DiscountHandler.getInstance().combineDiscounts(discounts, false);

                        Price<?> pay = tradeModule.getPayPrice(true).duplicate();
                        Price<?> gain = tradeModule.getGainPrice().duplicate();
                        pay.applyDiscount(discount);

                        balanceMessenger.handleTransaction(new Transaction(Customer.wrap(c.getPlayer()), e, EntryInteractionType.BUY,
                                Lists.newArrayList(pay), Lists.newArrayList(gain), LocalDateTime.now(), discount, discounts));
                    }
                    if (e.getModule() != null) {
                        StatShops.getInstance().getLogDatabase().logToDatabase(e.getModule().createLogEntry(Customer.wrap(c.getPlayer()), EntryInteractionResult.SUCCESS), this.villagerShop);
                    }
                }));
                trades.put(e, button);
            }
        }
    }

    @Override
    public void openSync(Player player, ViewMode viewMode) {
        prepareInventory(player);
        super.openSync(player, viewMode);
    }

    @Override
    public void close(Player viewer) {
        DiscountHandler.getInstance().unsubscribeToDisplayUpdates(this);
        this.balanceMessenger.handlePageClose(viewer);
        this.balanceMessenger.handleShopClose(viewer);
        super.close(viewer);
    }

    public void placeEntry(ShopEntry entry) {
        int index = getIndex(trades.get(entry));
        if (super.getMerchant() == null) {
            return;
        }
        MerchantRecipe recipe = super.getMerchant().getRecipe(index);
        //Limits
        Pair<Limit, Limit> limits = LimitsHandler.getInstance().getMinimalLimitsWithMatchingTags(targetCustomer.getPlayer(), entry, entry.getShop());
        Limit a = limits.getLeft();
        Limit b = limits.getRight();

        if (a != null || b != null) {
            long since;
            int limit;
            if (a != null && b != null) {
                since = System.currentTimeMillis() - Long.min(a.getRecover().toMillis(), b.getRecover().toMillis());
                limit = Integer.min(a.getTransactionLimit(), b.getTransactionLimit());
            } else if (a != null) {
                since = System.currentTimeMillis() - a.getRecover().toMillis();
                limit = a.getTransactionLimit();
            } else {
                since = System.currentTimeMillis() - b.getRecover().toMillis();
                limit = b.getTransactionLimit();
            }
            recipe.setUses(LimitsHandler.getInstance().getLimitUserCount(targetCustomer.getUuid(), entry.getUUID(), since, b != null));
            recipe.setMaxUses(limit);
        }


        //Discounts
        double discount = DiscountHandler.getInstance().combineDiscountsWithMatchingTags(targetCustomer.getPlayer(), false, entry, entry.getShop());
        recipe.setPriceMultiplier((float) discount);
        //TODO debug
        recipe.setMaxUses(discount < 1 ? 0 : 1);

        getMerchant().setRecipe(index, recipe);
    }

    @Override
    public void updateEntry(ShopEntry entry) {
        placeEntry(entry);
        viewer.keySet().stream().map(Bukkit::getPlayer).filter(Objects::nonNull).forEach(this::openSync);
    }

    @Override
    public void handleLimitRecoverInit(ShopEntry shopEntry, long recoverDuration) {
        //TODO
    }
}
