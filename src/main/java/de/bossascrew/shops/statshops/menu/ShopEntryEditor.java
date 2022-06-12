package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.data.Messages;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.statshops.shop.entry.DataSlot;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.cubbossa.menuframework.inventory.*;
import de.cubbossa.menuframework.inventory.implementations.AnvilMenu;
import de.cubbossa.menuframework.inventory.implementations.ListMenu;
import de.cubbossa.menuframework.inventory.implementations.RectInventoryMenu;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.*;
import java.util.stream.Collectors;

public class ShopEntryEditor extends RectInventoryMenu {

	private final ShopEntry entry;
	private final Player targetPlayer;
	@Getter
	private final Collection<Class<?>> allowedModuleTypes;

	public ShopEntryEditor(ShopEntry entry, Player targetPlayer) {
		super(Messages.GUI_SHOP_ENTRY, 3);
		this.entry = entry;
		this.targetPlayer = targetPlayer;
		this.closeHandlers.add(closeContext -> entry.saveToDatabase());
		this.allowedModuleTypes = EntryModuleHandler.getInstance().getEntryModules().values().stream()
				.filter(e -> e.getPermission() == null || targetPlayer.hasPermission(e.getPermission()))
				.map(EntryModuleHandler.EntryModuleProvider::getClass)
				.collect(Collectors.toSet());
	}

	private void prepare(ShopEntry entry) {

		removeAllPresets();
		addPreset(MenuPresets.back(2, 8, Action.LEFT));

		//TODO i suppose
		// Set deco lore
		setItem(0, ItemStackUtils.createItemStack(entry.getDisplayItem().getType(),
				Messages.GUI_ENTRY_SET_LORE_NAME, Messages.GUI_ENTRY_SET_LORE_LORE));

		setItemAndClickHandler(1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
						Messages.GUI_ENTRY_SET_PERMISSION_NAME, Messages.GUI_ENTRY_SET_PERMISSION_LORE.asComponents(
								TagResolver.resolver("permission", Tag.inserting(Component.text(entry.getPermission() == null ? "X" : entry.getPermission()))))),
				Action.LEFT, clickContext -> {
					AnvilMenu m = MainMenu.newAnvilMenu(Messages.GUI_ENTRY_SET_PERMISSION_TITLE, "shops.item.");
					m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
						entry.setPermission(s.getTarget());
						entry.saveToDatabase();
						m.openPreviousMenu(s.getPlayer());
					});
				});
		//Set tags
		setButton(2, Button.builder()
				.withItemStack(ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS, Messages.GUI_ENTRY_SET_TAGS_NAME, Messages.GUI_ENTRY_SET_TAGS_LORE))
				.withClickHandler(Action.LEFT, clickContext -> openSubMenu(clickContext.getPlayer(), MainMenu.newTagMenu(
						entry,
						Messages.GUI_TAGS_TITLE.asComponent(TagResolver.resolver("name", Tag.inserting(Component.text("shop entry")))),
						Messages.GUI_TAGS_NEW_TAG_TITLE, Messages.GUI_TAGS_NEW_TAG_NAME, Messages.GUI_TAGS_NEW_TAG_LORE,
						Messages.GENERAL_GUI_TAGS_REMOVE_TAG
				))));
		setButton(2, Button.builder()
				.withItemStack(ItemStackUtils.createItemStack(entry.getModule() == null ? new ItemStack(Material.BLACK_STAINED_GLASS) :
								entry.getModule().getDisplayItem(), Messages.GUI_ENTRY_SET_FUNCTION_NAME.asComponent(TagResolver.resolver("name", Tag.inserting(entry.getModule() == null ?
								Messages.GUI_ENTRY_FUNCTION_STATIC_NAME : entry.getModule().getDisplayName()))),
						Messages.GUI_ENTRY_SET_FUNCTION_LORE.asComponents(TagResolver.resolver("function", Tag.inserting(entry.getModule() == null ?
								Messages.GUI_ENTRY_FUNCTION_STATIC_NAME : entry.getModule().getDisplayName())))))
				.withClickHandler(Action.LEFT, clickContext -> openSubMenu(clickContext.getPlayer(), newEntryFunctionMenu())));

		if (entry.getModule() instanceof TradeModule tm) {
			setButton(9 + 2, Button.builder()
					.withItemStack(tm.getCosts().getProvider().getListDisplayItem())
					.withClickHandler(c -> openSubMenu(c.getPlayer(), () -> {
						ListMenu m = new ListMenu(Messages.GUI_ENTRY_SET_COSTS_TITLE, 3);
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
				limitsLore.add(Messages.SHOP_ITEM_LORE_SPACER.asComponent());
			}
			limitsLore.addAll(Messages.GUI_SHOP_SET_LIMITS_LORE.asComponents());

			setItemAndClickHandler(10, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Messages.GUI_SHOP_SET_LIMITS_NAME, limitsLore),
					Action.LEFT, c -> openSubMenu(c.getPlayer(), newShopLimitsMenu()));

			//Open Discounts menu
			List<Component> discountLore = new ArrayList<>();
			List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(targetPlayer, entry, entry.getShop());
			ItemStackUtils.addLoreDiscount(discountLore, discounts);
			if (discountLore.size() > 0) {
				discountLore.add(Messages.SHOP_ITEM_LORE_SPACER.asComponent());
			}
			discountLore.addAll(Messages.GUI_SHOP_SET_DISCOUNTS_LORE.asComponents());

			setItemAndClickHandler(19, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Messages.GUI_SHOP_SET_DISCOUNTS_NAME, discountLore),
					Action.LEFT, c -> openSubMenu(c.getPlayer(), newShopDiscountsMenu()));
		}

		int[] blackSlots = {3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23, 24, 25};
		for (int i : blackSlots) {
			setItem(i, Icon.STACK_EMPTY_DARK);
		}
		ItemStack glass_rp = Icon.STACK_EMPTY_DARK.clone();
		ItemStackUtils.setCustomModelData(glass_rp, entry.getModule() instanceof TradeModule ? 7122003 : 7122002);
		setItem(20, glass_rp);

		if (entry.getModule() != null) {

			Iterator<Integer> dataSlots = Arrays.stream(new int[]{3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 16, 17}).iterator();

			for (DataSlot<?> dataSlot : entry.getModule().getDataSlots()) {
				int slot = dataSlots.next();
				setItem(slot, Icon.STACK_EMPTY_MIDDLE);

				if (!dataSlots.hasNext()) {
					StatShops.getInstance().log(LoggingPolicy.WARN, "EntryEditor cannot display data slot because there are too many data slots.");
					continue;
				}
				// Data that cannot be edited via gui
				if (dataSlot == null || dataSlot.getDisplayItem() == null) {
					StatShops.getInstance().log(LoggingPolicy.WARN, "Entry was null or could not be displayed: " + dataSlot.getClass().getTypeName());
					continue;
				}
				setButton(slot, Button.builder().withItemStack(dataSlot::getDisplayItem).withClickHandler(dataSlot.getClickHandler()));
			}
		}
	}

	public TopMenu newEntryFunctionMenu() {
		ListEditorMenu<EntryModuleHandler.EntryModuleProvider> menu = new ListEditorMenu<>(Messages.GUI_ENTRY_SET_FUNCTION_TITLE, 3, new ListMenuSupplier<>() {

			public Collection<EntryModuleHandler.EntryModuleProvider> getElements() {
				return EntryModuleHandler.getInstance().getEntryModules().values().stream()
						.filter(e -> allowedModuleTypes.stream().anyMatch(aClass -> aClass.isInstance(e)))
						.filter(e -> (e.getPermission() == null || targetPlayer.hasPermission(e.getPermission())))
						.collect(Collectors.toSet());
			}

			public ItemStack getDisplayItem(EntryModuleHandler.EntryModuleProvider e) {
				return (entry.getModule() == null && e.getKey().equals("static")) ||
						(entry.getModule() != null && entry.getModule().getProvider().getKey().equals(e.getKey())) ?
						ItemStackUtils.setGlow(e.getListDisplayItem()) : e.getListDisplayItem();
			}
		});
		menu.setClickHandler(Action.LEFT, c -> entry.setModule(c.getTarget().getModule(entry)));
		return menu;
	}

	@Override
	public void openSync(Player player, ViewMode viewMode) {
		prepare(entry);
		super.openSync(player, viewMode);
	}

	public TopMenu newShopLimitsMenu() {
		ListEditorMenu<Limit> menu = new ListEditorMenu<>(Messages.GUI_SHOP_LIMITS_TITLE, 3, LimitsHandler.getInstance());
		menu.setItemModifier((limit, stack) -> TagUtils.hasCommonTags(entry, limit) || TagUtils.hasCommonTags(entry.getShop(), limit) ?
				ItemStackUtils.setGlow(stack) : stack);
		menu.setClickHandler(Action.LEFT, c -> {
			c.getTarget().addTag(entry.getUUID().toString());
			c.getMenu().refresh(c.getSlot());
		});
		menu.setClickHandler(Action.RIGHT, c -> {
			c.getTarget().removeTag(entry.getUUID().toString());
			c.getMenu().refresh(c.getSlot());
		});
		return menu;
	}

	public TopMenu newShopDiscountsMenu() {
		ListEditorMenu<Discount> menu = new ListEditorMenu<>(Messages.GUI_SHOP_DISCOUNTS_TITLE, 3, DiscountHandler.getInstance());
		menu.setItemModifier((discount, stack) -> TagUtils.hasCommonTags(entry, discount) || TagUtils.hasCommonTags(entry.getShop(), discount) ?
				ItemStackUtils.setGlow(stack) : stack);
		menu.setClickHandler(Action.LEFT, c -> {
			c.getTarget().addTag(entry.getUUID().toString());
			c.getMenu().refresh(c.getSlot());
		});
		menu.setClickHandler(Action.RIGHT, c -> {
			c.getTarget().removeTag(entry.getUUID().toString());
			c.getMenu().refresh(c.getSlot());
		});
		return menu;
	}
}
