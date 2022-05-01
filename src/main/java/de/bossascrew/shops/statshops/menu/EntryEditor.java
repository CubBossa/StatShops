package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.statshops.shop.entry.DataSlot;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.cubbossa.guiframework.inventory.Action;
import de.cubbossa.guiframework.inventory.Button;
import de.cubbossa.guiframework.inventory.Menu;
import de.cubbossa.guiframework.inventory.implementations.AnvilMenu;
import de.cubbossa.guiframework.inventory.implementations.ListMenu;
import de.cubbossa.guiframework.inventory.implementations.RectInventoryMenu;
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class EntryEditor extends RectInventoryMenu {

	private final ShopEntry entry;
	private final Player targetPlayer;
	@Getter
	@Setter
	@Nullable
	private Collection<Class<?>> allowedModuleTypes = null;

	public EntryEditor(ShopEntry entry, Player targetPlayer) {
		super(Message.GUI_SHOP_ENTRY, 3);
		this.entry = entry;
		this.targetPlayer = targetPlayer;
		this.closeHandler = closeContext -> entry.saveToDatabase();
	}

	private void prepare(ShopEntry entry) {
		//TODO i suppose
		// Set deco lore
		setItem(0, ItemStackUtils.createItemStack(entry.getDisplayItem().getType(),
				Message.GUI_ENTRY_SET_LORE_NAME, Message.GUI_ENTRY_SET_LORE_LORE));

		setItemAndClickHandler(1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
						Message.GUI_ENTRY_SET_PERMISSION_NAME, Message.GUI_ENTRY_SET_PERMISSION_LORE.getTranslations(
								TagResolver.resolver("permission", Tag.inserting(Component.text(entry.getPermission() == null ? "X" : entry.getPermission()))))),
				Action.LEFT, clickContext -> {
					AnvilMenu m = new AnvilMenu(Message.GUI_ENTRY_SET_PERMISSION_TITLE, "shops.item.");
					m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
						entry.setPermission(s.getTarget());
						entry.saveToDatabase();
						s.getPlayer().closeInventory();
					});
				});
		//Set tags
		setButton(2, Button.builder()
				.withItemStack(ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS, Message.GUI_ENTRY_SET_TAGS_NAME, Message.GUI_ENTRY_SET_TAGS_LORE))
				.withClickHandler(Action.LEFT, clickContext -> clickContext.getMenu().openSubMenu(clickContext.getPlayer(), StatShopMenus.newTagMenu(
						entry,
						Message.GUI_TAGS_TITLE.getTranslation(TagResolver.resolver("name", Tag.inserting(Component.text("shop entry")))),
						Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
						Message.GENERAL_GUI_TAGS_REMOVE_TAG
				))));
		setButton(2, Button.builder()
				.withItemStack(ItemStackUtils.createItemStack(entry.getModule() == null ? new ItemStack(Material.BLACK_STAINED_GLASS) :
								entry.getModule().getDisplayItem(), Message.GUI_ENTRY_SET_FUNCTION_NAME.getTranslation(TagResolver.resolver("name", Tag.inserting(entry.getModule() == null ?
								Message.GUI_ENTRY_FUNCTION_STATIC_NAME : entry.getModule().getDisplayName()))),
						Message.GUI_ENTRY_SET_FUNCTION_LORE.getTranslations(TagResolver.resolver("function", Tag.inserting(entry.getModule() == null ?
								Message.GUI_ENTRY_FUNCTION_STATIC_NAME : entry.getModule().getDisplayName())))))
				.withClickHandler(Action.LEFT, clickContext -> clickContext.getMenu().openSubMenu(clickContext.getPlayer(), () -> {

					ListMenu m = new ListMenu(Message.GUI_ENTRY_SET_FUNCTION_TITLE, 3);
					EntryModuleHandler.getInstance().getEntryModules().values().stream()
							.filter(e -> allowedModuleTypes.stream().anyMatch(aClass -> aClass.isInstance(e)))
							.filter(e -> (e.getPermission() == null || clickContext.getPlayer().hasPermission(e.getPermission())))
							.forEach(e -> m.addListEntry(Button.builder()
									.withItemStack(() -> (entry.getModule() == null && e.getKey().equals("static")) ||
											(entry.getModule() != null && entry.getModule().getProvider().getKey().equals(e.getKey())) ?
											ItemStackUtils.setGlow(e.getListDisplayItem()) : e.getListDisplayItem())
									.withClickHandler(Action.LEFT, c -> {
										entry.setModule(e.getModule(entry));
										m.refresh(m.getListSlots());
									})));
					return m;
				})));

		if (entry.getModule() instanceof TradeModule tm) {
			setButton(9 + 2, Button.builder()
					.withItemStack(tm.getCosts().getProvider().getListDisplayItem())
					.withClickHandler(c -> c.getMenu().openSubMenu(c.getPlayer(), () -> {
						ListMenu m = new ListMenu(Message.GUI_ENTRY_SET_COSTS_TITLE, 3);
						SubModulesHandler.getInstance().getValues().stream()
								.filter(p -> p.getPermission() == null || c.getPlayer().hasPermission(p.getPermission()))
								.forEach(p -> {
									m.addListEntry(Button.builder()
											.withItemStack(() -> tm.getCosts().getProvider().equals(p) ?
													ItemStackUtils.setGlow(p.getListDisplayItem()) : p.getListDisplayItem())
											.withClickHandler(Action.LEFT, cl -> tm.setCosts(p.getModule(entry))));
								});
						return m;
					})));

			//Open Limits menu
			List<Component> limitsLore = new ArrayList<>();
			Pair<Limit, Limit> limits = LimitsHandler.getInstance().getMinimalLimitsWithMatchingTags(targetPlayer, entry, entry.getShop());
			ItemStackUtils.addLoreLimits(limitsLore, limits.getLeft(), limits.getRight(), 0);
			if (limitsLore.size() > 0) {
				limitsLore.add(Message.SHOP_ITEM_LORE_SPACER.asComponent());
			}
			limitsLore.addAll(Message.GUI_SHOP_SET_LIMITS_LORE.getTranslations());

			setItemAndClickHandler(10, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Message.GUI_SHOP_SET_LIMITS_NAME, limitsLore),
					Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), newShopLimitsMenu()));

			//Open Discounts menu
			List<Component> discountLore = new ArrayList<>();
			List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(targetPlayer, entry, entry.getShop());
			ItemStackUtils.addLoreDiscount(discountLore, discounts);
			if (discountLore.size() > 0) {
				discountLore.add(Message.SHOP_ITEM_LORE_SPACER.asComponent());
			}
			discountLore.addAll(Message.GUI_SHOP_SET_DISCOUNTS_LORE.getTranslations());

			setItemAndClickHandler(19, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_SHOP_SET_DISCOUNTS_NAME, discountLore),
					Action.LEFT, c -> c.getMenu().openSubMenu(c.getPlayer(), newShopDiscountsMenu()));
		}

		int[] blackSlots = {3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23, 24, 25};
		for (int i : blackSlots) {
			setItem(i, Icon.EMPTY_DARK.create());
		}
		ItemStack glass_rp = Icon.EMPTY_DARK.create();
		ItemStackUtils.setCustomModelData(glass_rp, entry.getModule() instanceof TradeModule ? 7122003 : 7122002);
		setItem(20, glass_rp);

		if (entry.getModule() != null) {

			Iterator<Integer> dataSlots = Arrays.stream(new int[]{3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 16, 17}).iterator();

			for (DataSlot<?> dataSlot : entry.getModule().getDataSlots()) {

				if (!dataSlots.hasNext()) {
					StatShops.getInstance().log(LoggingPolicy.WARN, "EntryEditor cannot display data slot because there are too many data slots.");
					continue;
				}
				// Data that cannot be edited via gui
				if (dataSlot == null || dataSlot.getDisplayItem() == null) {
					StatShops.getInstance().log(LoggingPolicy.WARN, "Entry was null or could not be displayed: " + dataSlot.getClass().getTypeName());
					continue;
				}
				int slot = dataSlots.next();
				setButton(slot, Button.builder().withItemStack(dataSlot::getDisplayItem).withClickHandler(dataSlot.getClickHandler()));
			}
		}
	}

	@Override
	public void openSync(Player player, ViewMode viewMode) {
		prepare(entry);
		super.openSync(player, viewMode);
	}

	public Menu newShopLimitsMenu() {
		ListMenu menu = new ListMenu(Message.GUI_SHOP_LIMITS_TITLE, 3);
		for (Limit limit : LimitsHandler.getInstance().getLimits()) {
			menu.addListEntry(Button.builder()
					.withItemStack(() -> TagUtils.hasCommonTags(entry, limit) || TagUtils.hasCommonTags(entry.getShop(), limit) ?
							ItemStackUtils.setGlow(limit.getListDisplayItem()) : limit.getListDisplayItem())
					.withClickHandler(Action.LEFT, c -> {
						limit.addTag(entry.getUUID().toString());
						c.getMenu().refresh(c.getSlot());
					})
					.withClickHandler(Action.RIGHT, c -> {
						limit.removeTag(entry.getUUID().toString());
						c.getMenu().refresh(c.getSlot());
					}));
		}
		return menu;
	}

	public Menu newShopDiscountsMenu() {
		ListMenu menu = new ListMenu(Message.GUI_SHOP_DISCOUNTS_TITLE, 3);
		for (Discount discount : DiscountHandler.getInstance().getDiscounts()) {
			menu.addListEntry(Button.builder()
					.withItemStack(() -> TagUtils.hasCommonTags(entry, discount) || TagUtils.hasCommonTags(entry.getShop(), discount) ?
							ItemStackUtils.setGlow(discount.getListDisplayItem()) : discount.getListDisplayItem())
					.withClickHandler(Action.LEFT, c -> {
						discount.addTag(entry.getUUID().toString());
						c.getMenu().refresh(c.getSlot());
					})
					.withClickHandler(Action.RIGHT, c -> {
						discount.removeTag(entry.getUUID().toString());
						c.getMenu().refresh(c.getSlot());
					}));
		}
		return menu;
	}
}
