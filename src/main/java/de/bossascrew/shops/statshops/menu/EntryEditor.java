package de.bossascrew.shops.statshops.menu;

import de.bossascrew.shops.general.menu.ChestMenu;
import de.bossascrew.shops.general.menu.DefaultSpecialItem;
import de.bossascrew.shops.general.menu.EditorMenu;
import de.bossascrew.shops.general.menu.contexts.BackContext;
import de.bossascrew.shops.general.menu.contexts.ContextConsumer;
import de.bossascrew.shops.general.menu.contexts.TargetContext;
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
import lombok.Getter;
import lombok.Setter;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.minimessage.tag.Tag;
import net.kyori.adventure.text.minimessage.tag.resolver.TagResolver;
import net.wesjd.anvilgui.AnvilGUI;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryView;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.function.Consumer;

public class EntryEditor extends ChestMenu implements EditorMenu<Player> {

	private final ShopEntry entry;
	private final ContextConsumer<BackContext> backHandler;
	@Getter
	@Setter
	@Nullable
	private Collection<Class<?>> allowedModuleTypes = null;

	public EntryEditor(ShopEntry entry, ContextConsumer<BackContext> backHandler) {
		super(Message.GUI_SHOP_ENTRY, 3);
		this.entry = entry;
		this.backHandler = backHandler;
		this.closeHandler = closeContext -> {
			entry.saveToDatabase();
			setEditor(null);
		};
	}

	private void prepareMenu(Player targetPlayer) {
		fillMenu(null, DefaultSpecialItem.EMPTY_LIGHT_RP);
		setBackHandlerAction(backHandler);
		//Set deco lore
		setItemAndClickHandler(0, 0, ItemStackUtils.createItemStack(entry.getDisplayItem().getType(),
				Message.GUI_ENTRY_SET_LORE_NAME, Message.GUI_ENTRY_SET_LORE_LORE), clickContext -> {

		});
		//Set permissions
		setItemAndClickHandler(1, 0, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_PERMISSIONS,
				Message.GUI_ENTRY_SET_PERMISSION_NAME.getTranslation(), Message.GUI_ENTRY_SET_PERMISSION_LORE.getTranslations(
						TagResolver.resolver("permission", Tag.inserting(Component.text(entry.getPermission() == null ? "X" : entry.getPermission())))
				)), clickContext -> {
			Player player = clickContext.getPlayer();
			player.closeInventory();
			new AnvilGUI.Builder()
					.plugin(StatShops.getInstance())
					.text("shops.item.")
					.title(Message.GUI_ENTRY_SET_PERMISSION_TITLE.getLegacyTranslation())
					.onClose(p -> Bukkit.getScheduler().runTaskLater(StatShops.getInstance(), () -> openInventory(p), 1L))
					.onComplete((p, s) -> {
						entry.setPermission(s);
						entry.saveToDatabase();
						openInventory(player);
						return AnvilGUI.Response.close();
					}).open(player);
		});
		//Set tags
		setItemAndClickHandler(2, 0, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_TAGS,
						Message.GUI_ENTRY_SET_TAGS_NAME, Message.GUI_ENTRY_SET_TAGS_LORE),
				clickContext -> {
					Player player = clickContext.getPlayer();
					TagsEditorMenu<ShopEntry> menu = new TagsEditorMenu<>(entry,
							Message.GUI_TAGS_TITLE.getTranslation(TagResolver.resolver("name", Tag.inserting(Component.text("shop entry")))),
							Message.GUI_TAGS_NEW_TAG_TITLE, Message.GUI_TAGS_NEW_TAG_NAME, Message.GUI_TAGS_NEW_TAG_LORE,
							Message.GENERAL_GUI_TAGS_REMOVE_TAG, backContext -> openInventory(player));
					menu.openInventory(player);
				});

		setItemAndClickHandler(0, 2, ItemStackUtils.createItemStack(entry.getModule() == null ? new ItemStack(Material.BLACK_STAINED_GLASS) :
						entry.getModule().getDisplayItem(), Message.GUI_ENTRY_SET_FUNCTION_NAME.getTranslation(TagResolver.resolver("name", Tag.inserting(entry.getModule() == null ?
						Message.GUI_ENTRY_FUNCTION_STATIC_NAME.getTranslation() : entry.getModule().getDisplayName()))),
				Message.GUI_ENTRY_SET_FUNCTION_LORE.getTranslations(TagResolver.resolver("function", Tag.inserting(entry.getModule() == null ?
						Message.GUI_ENTRY_FUNCTION_STATIC_NAME.getTranslation() : entry.getModule().getDisplayName())))), clickContext -> {

			LMenu<EntryModuleHandler.EntryModuleProvider> listMenu = new LMenu<>(3, EntryModuleHandler.getInstance(),
					Message.GUI_ENTRY_SET_FUNCTION_TITLE, backContext -> openInventory(clickContext.getPlayer()));

			listMenu.setDisplayPredicate(provider -> (allowedModuleTypes == null
					|| allowedModuleTypes.stream().anyMatch(aClass -> aClass.isInstance(provider))
					&& (provider.getPermission() == null || clickContext.getPlayer().hasPermission(provider.getPermission()))));
			listMenu.setGlowPredicate(provider -> (entry.getModule() == null && provider.getKey().equals("static")) ||
					(entry.getModule() != null && entry.getModule().getProvider().getKey().equals(provider.getKey())));
			listMenu.setClickHandler(cc -> {
				entry.setModule(cc.getTarget().getModule(entry));
				openInventory(cc.getPlayer());
			});
			listMenu.openInventory(clickContext.getPlayer());
		});
		if (entry.getModule() instanceof TradeModule tm) {
			setItemAndClickHandler(1, 2, tm.getCosts().getProvider().getListDisplayItem(), clickContext -> {
				LMenu<SubModulesHandler.CostsSubModuleProvider<?>> listMenu = new LMenu<>(3, SubModulesHandler.getInstance(),
						Message.GUI_ENTRY_SET_COSTS_TITLE, backContext -> openInventory(clickContext.getPlayer()));
				listMenu.setDisplayPredicate(provider -> provider.getPermission() == null || clickContext.getPlayer().hasPermission(provider.getPermission()));
				listMenu.setGlowPredicate(s -> tm.getCosts().getProvider().equals(s));
				listMenu.setClickHandler(cc -> {
					tm.setCosts(cc.getTarget().getModule(entry));
					openInventory(cc.getPlayer());
				});
				listMenu.openInventory(clickContext.getPlayer());
			});

			//Open Limits menu
			List<Component> limitsLore = new ArrayList<>();
			Pair<Limit, Limit> limits = LimitsHandler.getInstance().getMinimalLimitsWithMatchingTags(targetPlayer, entry, entry.getShop());
			ItemStackUtils.addLoreLimits(limitsLore, limits.getLeft(), limits.getRight(), 0);
			if (limitsLore.size() > 0) {
				limitsLore.add(Message.SHOP_ITEM_LORE_SPACER.getTranslation());
			}
			limitsLore.addAll(Message.GUI_SHOP_SET_LIMITS_LORE.getTranslations());
			setItemAndClickHandler(1, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_LIMIT,
							Message.GUI_SHOP_SET_LIMITS_NAME.getTranslation(), limitsLore),
					clickContext -> openShopLimitsMenu(clickContext.getPlayer(), 0));

			//Open Discounts menu
			List<Component> discountLore = new ArrayList<>();
			List<Discount> discounts = DiscountHandler.getInstance().getDiscountsWithMatchingTags(targetPlayer, entry, entry.getShop());
			ItemStackUtils.addLoreDiscount(discountLore, discounts);
			if (discountLore.size() > 0) {
				discountLore.add(Message.SHOP_ITEM_LORE_SPACER.getTranslation());
			}
			discountLore.addAll(Message.GUI_SHOP_SET_DISCOUNTS_LORE.getTranslations());
			setItemAndClickHandler(2, 1, ItemStackUtils.createItemStack(ItemStackUtils.MATERIAL_DISCOUNT,
							Message.GUI_SHOP_SET_DISCOUNTS_NAME.getTranslation(), discountLore),
					clickContext -> openShopDiscountsMenu(clickContext.getPlayer(), 0));
		}

		int[] blackSlots = {3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23, 24, 25};
		for (int i : blackSlots) {
			setItem(i, DefaultSpecialItem.EMPTY_DARK.createSpecialItem());
		}
		ItemStack glass_rp = DefaultSpecialItem.EMPTY_DARK.createSpecialItem();
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
				setItemAndClickHandler(slot, dataSlot.getDisplayItem(), clickContext -> {
					dataSlot.getClickHandler().accept(new TargetContext<>(clickContext, () -> openInventory(clickContext.getPlayer())));
					setItem(slot, dataSlot.getDisplayItem());
					refresh(slot);
				});
			}
		}
	}

	@Override
	public InventoryView openInventorySync(@NotNull Player player, @Nullable Consumer<Inventory> inventoryPreparer) {
		setEditor(player);
		prepareMenu(player);
		return super.openInventorySync(player, inventoryPreparer);
	}

	@Override
	public boolean isEditorSet() {
		return entry.getShop().getEditor() != null;
	}

	@Override
	public boolean setEditor(Player editor) {
		if (isEditorSet()) {
			return false;
		}
		entry.getShop().setEditor(editor);
		return true;
	}

	@Override
	public Player getEditor() {
		return entry.getShop().getEditor();
	}

	public void openShopLimitsMenu(Player player, int page) {
		LMenu<Limit> listMenu = new LMenu<>(3, LimitsHandler.getInstance(), Message.GUI_SHOP_LIMITS_TITLE, backContext -> openInventory(player));
		listMenu.setNavigationEntry(4, ItemStackUtils.createInfoItem(Message.GUI_SHOP_LIMITS_INFO_NAME, Message.GUI_SHOP_LIMITS_INFO_LORE), clickContext -> {
		});
		listMenu.setGlowPredicate(limit -> TagUtils.hasCommonTags(entry, limit) || TagUtils.hasCommonTags(entry.getShop(), limit));
		listMenu.setClickHandler(targetContext -> {
			Limit limit = targetContext.getTarget();
			if (targetContext.getAction().isRightClick()) {
				limit.removeTag(entry.getUUID().toString());
			} else if (targetContext.getAction().isLeftClick()) {
				limit.addTag(entry.getUUID().toString());
			}
			listMenu.openInventory(player, listMenu.getCurrentPage());
		});
		listMenu.openInventory(player, page);
	}

	public void openShopDiscountsMenu(Player player, int page) {
		LMenu<Discount> listMenu = new LMenu<>(3, DiscountHandler.getInstance(), Message.GUI_SHOP_DISCOUNTS_TITLE, backContext -> openInventory(player));
		listMenu.setNavigationEntry(4, ItemStackUtils.createInfoItem(Message.GUI_SHOP_DISCOUNTS_INFO_NAME, Message.GUI_SHOP_DISCOUNTS_INFO_LORE), clickContext -> {
		});
		listMenu.setGlowPredicate(discount -> TagUtils.hasCommonTags(entry.getShop(), discount) || TagUtils.hasCommonTags(entry, discount));
		listMenu.setClickHandler(targetContext -> {
			Discount discount = targetContext.getTarget();
			if (targetContext.getAction().isRightClick()) {
				discount.removeTag(entry.getUUID().toString());
			} else if (targetContext.getAction().isLeftClick()) {
				discount.addTag(entry.getUUID().toString());
			}
			listMenu.openInventory(player, listMenu.getCurrentPage());
		});
		listMenu.openInventory(player, page);
	}
}
