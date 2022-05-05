package de.bossascrew.shops.statshops.handler;

import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.StatShopsExtension;
import de.bossascrew.shops.statshops.api.PaginatedShop;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.api.data.Database;
import de.bossascrew.shops.statshops.data.Message;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.VillagerShop;
import de.bossascrew.shops.statshops.util.ItemStackUtils;
import de.bossascrew.shops.web.WebAccessable;
import de.cubbossa.menuframework.inventory.ListMenuSupplier;
import lombok.Getter;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.stream.Collectors;

public class ShopHandler implements
		WebAccessable<Shop>,
		ListMenuSupplier<Shop> {

	public record ShopTypeProvider(Class<? extends Shop> type,
								   Material displayMaterial, Message name,
								   Message lore) {
	}

	@Getter
	private static ShopHandler instance;

	@Getter
	private final Map<UUID, Shop> shopMap;
	@Getter
	private final List<ShopTypeProvider> registeredShopTypes;

	public ShopHandler() {
		instance = this;

		this.shopMap = new HashMap<>();
		this.registeredShopTypes = new ArrayList<>();

		registeredShopTypes.add(new ShopTypeProvider(ChestMenuShop.class, Material.CHEST, Message.SHOP_TYPE_CHEST_NAME, Message.SHOP_TYPE_CHEST_LORE));
		registeredShopTypes.add(new ShopTypeProvider(VillagerShop.class, Material.VILLAGER_SPAWN_EGG, Message.SHOP_TYPE_VILLAGER_NAME, Message.SHOP_TYPE_VILLAGER_LORE));

		for (StatShopsExtension extension : StatShops.getRegisteredExtensions()) {
			extension.registerShopTypes(this);
		}
	}

	public void registerShopType(ShopTypeProvider shopTypeProvider) throws NoSuchMethodException {
		// Warn extension developers on enabling
		shopTypeProvider.type().getConstructor(UUID.class, String.class);
		this.registeredShopTypes.add(shopTypeProvider);
	}

	public void loadShopsFromDatabase(Database database) {
		this.shopMap.putAll(database.loadShops());
	}

	public List<Shop> getShops() {
		return shopMap.values().stream().sorted().collect(Collectors.toList());
	}

	public Shop getShop(UUID uuid) {
		return shopMap.get(uuid);
	}

	public @Nullable <T extends Shop> T createShop(String nameFormat, Class<T> type) {
		try {
			Constructor<? extends T> constructor = type.getConstructor(UUID.class, String.class);
			T shop = constructor.newInstance(UUID.randomUUID(), nameFormat);
			StatShops.getInstance().getDatabase().saveShop(shop);
			addShop(shop);
			return shop;
		} catch (NoSuchMethodException | InvocationTargetException | InstantiationException | IllegalAccessException e) {
			throw new IllegalArgumentException("The provided Shop Type could not be constructed by using a Constructor(UUID, String)", e);
		}
	}

	public void addShop(Shop shop) {
		shopMap.put(shop.getUUID(), shop);
	}

	public boolean deleteShop(Shop shop) {
		shop.closeAll();
		StatShops.getInstance().getDatabase().deleteShop(shop);
		if (StatShops.getInstance().isCitizensInstalled()) {
			StatShops.getInstance().getCitizensHook().removeAllAssignments(shop);
		}
		return shopMap.remove(shop.getUUID()) != null;
	}

	@Override
	public List<Shop> getWebData() {
		return getShops();
	}

	@Override
	public void storeWebData(List<Shop> values) {
		//TODO
	}

	public List<Shop> getValues() {
		return getShops();
	}

	public Shop createNew(String input) {
		return createNew(input, getTypes().get(0).type());
	}

	public <T extends Shop> T createNew(String input, Class<T> type) {
		return createShop(input, type);
	}

	public List<ShopTypeProvider> getTypes() {
		return registeredShopTypes;
	}

	@Override
	public Collection<Shop> getElements() {
		return getShops();
	}

	@Override
	public ItemStack getDisplayItem(Shop object) {
		return ItemStackUtils.createShopItemStack(object);
	}

	public Shop createDuplicate(Shop element) {
		Shop shop = createShop(element.getNameFormat(), element.getClass());
		shop.setDisplayItem(element.getDisplayItem());
		shop.setPermission(element.getPermission());
		if (element instanceof PaginatedShop ps) { //TODO pro shop einen copy constructor -> via reflection aufrufen
			ps.setDefaultShopPage(ps.getDefaultShopPage());
			ps.setPageRemembered(ps.isPageRemembered());
		}
		for (String tag : element.getTags()) {
			shop.addTag(tag);
		}
		for (Map.Entry<Integer, ShopEntry> entry : element.getEntries().entrySet()) {
			shop.addEntry(entry.getValue().duplicate(), entry.getKey());
		}
		StatShops.getInstance().getDatabase().saveShop(shop);
		return shop;
	}
}
