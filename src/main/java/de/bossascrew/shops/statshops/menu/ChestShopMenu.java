package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.ShopMenu;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.data.Customer;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.EntryInteractionResult;
import de.bossascrew.shops.statshops.util.EntryInteractionType;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.cubbossa.menuframework.inventory.Button;
import de.cubbossa.menuframework.inventory.implementations.RectInventoryMenu;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.scheduler.BukkitTask;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Collectors;

public class ChestShopMenu extends RectInventoryMenu implements ShopMenu {

	private final ChestMenuShop shop;
	private final Customer targetCustomer;
	private final int cooldown;
	private final boolean showCooldownMessage;
	private final HashMap<Player, Long> interactionCooldown;

	private final List<BukkitTask> schedulers;

	public ChestShopMenu(ChestMenuShop shop, Customer customer) {
		super(shop.getName(), shop.getRows());
		this.shop = shop;
		this.interactionCooldown = new HashMap<>();
		this.cooldown = StatShops.getInstance().getShopsConfig().getCooldown();
		this.showCooldownMessage = StatShops.getInstance().getShopsConfig().isShowCooldownMessage();
		this.targetCustomer = customer;
		this.schedulers = new ArrayList<>();

		setupEntries(targetCustomer.getPlayer());
	}

	@Override
	public Inventory getInventory() {
		return Bukkit.createInventory(null, getSlots().length, TextUtils.toLegacy(
				Messages.SHOP_GUI_TITLE.format(TagResolver.builder()
						.tag("name", Tag.inserting(shop.getName()))
						.tag("page-title", Tag.inserting(shop.getPageTitle(getCurrentPage())))
						.tag("page", Tag.inserting(Component.text(getCurrentPage() + 1)))
						.tag("pages", Tag.inserting(Component.text(shop.getPageCount())))
						.build()
				).asComponent()));
	}

	public void setupEntries(Player player) {

		for (ShopEntry entry : shop.getEntries().values()) {
			setEntry(entry);

			//Subscribe to limits and discounts so changes can be displayed live
			DiscountHandler.getInstance().subscribeToDisplayUpdates(this, entry);
			LimitsHandler.getInstance().subscribeToDisplayUpdates(this, player, entry);
		}

		Customer customer = Customer.wrap(player);
		customer.setPage(shop, getCurrentPage());

		long now = System.currentTimeMillis();
		for (ShopEntry entry : shop.getEntries().values()) {
			if (!(entry.getModule() instanceof TradeModule)) {
				continue;
			}
			for (LimitsHandler.EntryInteraction interaction : LimitsHandler.getInstance().getExpiringInteractions(entry.getUUID())) {
				handleLimitRecoverInit(entry, interaction.timeStamp() + interaction.duration() - now);
			}
		}
	}

	@Override
	public void lastClose() {
		super.lastClose();
		unsubscribeToDisplayUpdates();
		cancelLimitRecoveryTasks();
	}

	private void setupLimitRecoveryScheduler(ShopEntry shopEntry, int durationInTicks) {
		BukkitTask task = Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> {
			updateEntry(shopEntry);
			cleanupRecoveryScheduler();
		}, durationInTicks + 2);
		schedulers.add(task);
	}

	private void cleanupRecoveryScheduler() {
		List<BukkitTask> remove = schedulers.stream().filter(BukkitTask::isCancelled).collect(Collectors.toList());
		schedulers.removeAll(remove);
	}

	public void handleLimitRecoverInit(ShopEntry shopEntry, long recoverDuration) {
		setupLimitRecoveryScheduler(shopEntry, (int) (recoverDuration / 50));
	}

	private void cancelLimitRecoveryTasks() {
		schedulers.forEach(BukkitTask::cancel);
		schedulers.clear();
	}

	public void unsubscribeToDisplayUpdates() {
		DiscountHandler.getInstance().unsubscribeToDisplayUpdates(this);
		LimitsHandler.getInstance().unsubscribeToDisplayUpdates(this);
	}

	public void setEntry(ShopEntry entry) {
		setButton(entry.getSlot() % (6 * 9), Button.builder()
				.withItemStack(() -> ItemStackUtils.createEntryItemStack(entry, targetCustomer))
				.withClickHandler(clickContext -> {
					Player player = clickContext.getPlayer();
					Customer c = Customer.wrap(player);

					long now = System.currentTimeMillis();
					Long last = interactionCooldown.get(player);
					if (last != null) {
						long dif = now - last;
						if (dif < cooldown) {
							if (showCooldownMessage) {
								c.sendMessage(Messages.SHOP_COOLDOWN);
							}
							return;
						}
					}
					EntryInteractionResult result = entry.interact(targetCustomer, this, EntryInteractionType.fromAction(clickContext.getAction()));
					if (result == EntryInteractionResult.SUCCESS && entry.getModule() != null && entry.getModule() instanceof TradeModule tm) {
						shop.getBalanceMessenger().handleTransaction(tm.getLastTransaction(targetCustomer));
					}
					interactionCooldown.put(player, now);
				}));
	}

	public void updateEntry(ShopEntry entry) {
		refresh(entry.getSlot() % (6 * 9));
	}
}
