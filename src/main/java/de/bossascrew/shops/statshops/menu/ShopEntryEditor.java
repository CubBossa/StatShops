package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.util.Pair;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.module.TradeModule;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.EntryModuleHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.handler.SubModulesHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.cubbossa.menuframework.inventory.*;
import de.cubbossa.menuframework.inventory.implementations.AnvilMenu;
import de.cubbossa.menuframework.inventory.implementations.ListMenu;
import lombok.Getter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class ShopEntryEditor extends DataHolderEditorMenu {

	private final ShopEntry entry;
	private final Player targetPlayer;
	@Getter
	private final Collection<Class<?>> allowedModuleTypes;

	public ShopEntryEditor(ShopEntry entry, Player targetPlayer) {
		super(Message.GUI_SHOP_ENTRY, 1, entry);
		this.entry = entry;
		this.targetPlayer = targetPlayer;
		this.closeHandler = closeContext -> entry.saveToDatabase();
		this.allowedModuleTypes = EntryModuleHandler.getInstance().getEntryModules().values().stream()
				.filter(e -> e.getPermission() == null || targetPlayer.hasPermission(e.getPermission()))
				.map(EntryModuleHandler.EntryModuleProvider::getClass)
				.collect(Collectors.toSet());

		this.setDataProvider(e -> {
			if (e instanceof ShopEntry ee && ee.getModule() != null) {
				return List.of(ee.getModule().getDataSlots());
			}
			return new ArrayList<>();
		});

		addPreset(presetApplier -> presetApplier.addItem((getRows() - 3) * 9 + 5, () -> {
			return ItemStackUtils.setCustomModelData(Icon.STACK_EMPTY_DARK.clone(), 7122002);
		}));


		addPreset(presetApplier -> {

			// Set deco lore
			presetApplier.addItemOnTop(0, () -> ItemStackUtils.createItemStack(entry.getDisplayItem().getType(),
					Message.GUI_ENTRY_SET_LORE_NAME, Message.GUI_ENTRY_SET_LORE_LORE));

			// Set permissions
			presetApplier.addItemOnTop(1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
					Message.GUI_ENTRY_SET_PERMISSION_NAME, Message.GUI_ENTRY_SET_PERMISSION_LORE.asComponents(
							TagResolver.resolver("permission", Tag.inserting(Component.text(entry.getPermission() == null ? "X" : entry.getPermission()))))));
			presetApplier.addClickHandlerOnTop(1, Action.LEFT, clickContext -> {
				AnvilMenu m = MainMenu.newAnvilMenu(Message.GUI_ENTRY_SET_PERMISSION_TITLE, "shops.item.", AnvilInputValidator.VALIDATE_PERMISSION);
				m.setOutputClickHandler(AnvilMenu.CONFIRM, s -> {
					entry.setPermission(s.getTarget().trim());
					entry.saveToDatabase();
					m.openPreviousMenu(s.getPlayer());
				});
			});

			//Set tags
			presetApplier.addItemOnTop(3, () -> ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS, Message.GUI_ENTRY_SET_TAGS_NAME, Message.GUI_ENTRY_SET_TAGS_LORE));
			presetApplier.addClickHandlerOnTop(3, Action.LEFT, clickContext -> openSubMenu(clickContext.getPlayer(), MainMenu.newTagMenu(
					entry,
					Message.GUI_TAGS_TITLE.asComponent(TagResolver.resolver("name", Tag.inserting(Component.text("shop entry")))),
					Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
					Message.GENERAL_GUI_TAGS_REMOVE_TAG
			)));

			//Open Limits menu
			presetApplier.addItemOnTop(4, () -> {
				List<Component> limitsLore = new ArrayList<>();
				Pair<Limit, Limit> limits = LimitsHandler.getInstance().getMinimalLimitsWithMatchingTags(null, entry);
				ItemStackUtils.addLoreLimits(limitsLore, limits.getLeft(), limits.getRight(), 0);
				if (limitsLore.size() > 0) {
					limitsLore.add(Message.SHOP_ITEM_LORE_SPACER.asComponent());
				}
				limitsLore.addAll(Message.GUI_SHOP_SET_LIMITS_LORE.asComponents());
				return ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT, Message.GUI_SHOP_SET_LIMITS_NAME, limitsLore);
			});
			presetApplier.addClickHandlerOnTop(4, Action.LEFT, c -> openSubMenu(c.getPlayer(), newShopLimitsMenu()));

			//Open Discounts menu
			presetApplier.addItemOnTop(5, () -> {
				List<Component> discountLore = new ArrayList<>();
				List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(null, entry);
				ItemStackUtils.addLoreDiscount(discountLore, discounts);
				if (discountLore.size() > 0) {
					discountLore.add(Message.SHOP_ITEM_LORE_SPACER.asComponent());
				}
				discountLore.addAll(Message.GUI_SHOP_SET_DISCOUNTS_LORE.asComponents());
				return ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT, Message.GUI_SHOP_SET_DISCOUNTS_NAME, discountLore);
			});
			presetApplier.addClickHandlerOnTop(5, Action.LEFT, c -> openSubMenu(c.getPlayer(), newShopDiscountsMenu()));
		});


		addPreset(presetApplier -> {
			presetApplier.addItemOnTop(9 + 7, () -> ItemStackUtils.createItemStack(entry.getModule() == null ?
							new ItemStack(Material.BLACK_STAINED_GLASS) : entry.getModule().getDisplayItem(),
					Message.GUI_ENTRY_SET_FUNCTION_NAME.asComponent(TagResolver.resolver("name", Tag.inserting(entry.getModule() == null ?
							Message.GUI_ENTRY_FUNCTION_STATIC_NAME : entry.getModule().getDisplayName()))),
					Message.GUI_ENTRY_SET_FUNCTION_LORE.asComponents(TagResolver.resolver("function", Tag.inserting(entry.getModule() == null ?
							Message.GUI_ENTRY_FUNCTION_STATIC_NAME : entry.getModule().getDisplayName())))));
			presetApplier.addClickHandlerOnTop(9 + 7, Action.LEFT, clickContext -> openSubMenu(clickContext.getPlayer(), newEntryFunctionMenu()));

			if (entry.getModule() instanceof TradeModule tm) {
				presetApplier.addItemOnTop(9 + 6, () -> tm.getCosts().getProvider().getListDisplayItem());
				presetApplier.addClickHandlerOnTop(9 + 6, Action.LEFT, c -> openSubMenu(c.getPlayer(), () -> {
					ListMenu m = new ListMenu(Message.GUI_ENTRY_SET_COSTS_TITLE, 3);
					m.addPreset(MainMenu.bottomRow(2));
					m.addPreset(MenuPresets.back(2, 8, Action.LEFT));
					SubModulesHandler.getInstance().getValues().stream()
							.filter(p -> p.getPermission() == null || c.getPlayer().hasPermission(p.getPermission()))
							.forEach(p -> {
								m.addListEntry(Button.builder()
										.withItemStack(() -> tm.getCosts().getProvider().equals(p) ?
												ItemStackUtils.setGlow(p.getListDisplayItem()) : p.getListDisplayItem())
										.withClickHandler(Action.LEFT, cl -> tm.setCosts(p.getModule(entry))));
							});
					return m;
				}));
			}
		});
	}

	public TopMenu newEntryFunctionMenu() {
		ListEditorMenu<EntryModuleHandler.EntryModuleProvider> menu = new ListEditorMenu<>(Message.GUI_ENTRY_SET_FUNCTION_TITLE, 3, new ListMenuSupplier<>() {

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

	public TopMenu newShopLimitsMenu() {
		ListEditorMenu<Limit> menu = new ListEditorMenu<>(Message.GUI_SHOP_LIMITS_TITLE, 3, LimitsHandler.getInstance());
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
		ListEditorMenu<Discount> menu = new ListEditorMenu<>(Message.GUI_SHOP_DISCOUNTS_TITLE, 3, DiscountHandler.getInstance());
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
