package de.bossascrew.shops.statshops.data;

import de.bossascrew.shops.general.Customer;
import de.bossascrew.shops.general.Shop;
import de.bossascrew.shops.general.entry.ShopEntry;
import de.bossascrew.shops.general.handler.EntryModuleHandler;
import de.bossascrew.shops.general.handler.TemplateHandler;
import de.bossascrew.shops.general.util.ItemStackUtils;
import de.bossascrew.shops.general.util.TextUtils;
import de.bossascrew.shops.statshops.shop.ChestMenuShop;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.Limit;
import de.bossascrew.shops.statshops.shop.entry.BaseEntry;
import net.kyori.adventure.text.minimessage.MiniMessage;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.PotionMeta;
import org.bukkit.potion.PotionData;
import org.bukkit.potion.PotionType;

import java.time.Duration;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.*;

public class TestDatabase implements Database, LogDatabase {

	@Override
	public Customer loadCustomer(UUID uuid) {
		return new Customer(Bukkit.getPlayer(uuid), new HashMap<>());
	}

	@Override
	public void saveCustomer(Customer customer) {

	}

	public Shop createShop(String nameFormat, UUID uuid) {
		return new ChestMenuShop(nameFormat, uuid);
	}

	@Override
	public Map<UUID, Shop> loadShops() {
		Map<UUID, Shop> map = new HashMap<>();
		ChestMenuShop s1 = (ChestMenuShop) createShop("<rainbow>ExampleShop</rainbow>", UUID.randomUUID());
		s1.addTag("swords");
		s1.addTag("rainbow");
		s1.setDefaultTemplate(TemplateHandler.getInstance().getDefaultTemplate());
		s1.applyDefaultTemplate(TemplateHandler.getInstance().getDefaultTemplate(), 5);

		ShopEntry entry1 = createEntry(UUID.randomUUID(), s1, new ItemStack(Material.MINECART), 0);
		ItemStack article = new ItemStack(Material.MINECART);
		ItemMeta meta = article.getItemMeta();
		meta.addEnchant(Enchantment.ARROW_INFINITE, 2, true);
		article.setItemMeta(meta);

		entry1.setModule(EntryModuleHandler.getInstance().getEntryModules().get("trade_item").getModule(entry1));
		entry1.addTag("test");
		s1.addEntry(entry1, 0);

		ShopEntry entry2 = createEntry(UUID.randomUUID(), s1, new ItemStack(Material.POTION), 1);
		ItemStack article2 = new ItemStack(Material.POTION);
		PotionMeta pmeta = (PotionMeta) article2.getItemMeta();
		pmeta.setDisplayName(TextUtils.toLegacy(MiniMessage.get().parse("<rainbow>Huiiiiiiiii")));
		pmeta.setBasePotionData(new PotionData(PotionType.SPEED));
		article2.setItemMeta(pmeta);

		entry2.setModule(EntryModuleHandler.getInstance().getEntryModules().get("trade_item").getModule(entry2));
		//entry2.setModule(EntryModuleHandler.tradeItemMoney(entry2, article2, "6 / 2 * <db:diamond>"));
		entry2.addTag("test");
		s1.addEntry(entry2, 1);

		Shop s2 = createShop("<white>Boring Shop", UUID.randomUUID());

		/*Shop s3 = new VillagerShop("<dark_purple>Villager Shop", UUID.randomUUID());
		ShopEntry entry = createEntry(UUID.randomUUID(), s3, new ItemStack(Material.MINECART), ShopHandler.getInstance().getShopModes().get(0), 0);
		entry.setModule(EntryModuleHandler.getInstance().getEntryModules().get("trade_item").getModule(entry));
		//entry.setModule(EntryModuleHandler.tradeItemItem(entry, new ItemStack(Material.MINECART), new ItemStack(Material.EMERALD, 2)));
		entry.addTag("test");
		s3.newEntry(0, entry);
*/
		map.put(s1.getUUID(), s1);
		map.put(s2.getUUID(), s2);
//		map.put(s3.getUUID(), s3);
		return map;
	}

	@Override
	public void saveShop(Shop shop) {

	}

	@Override
	public void deleteShop(Shop shop) {

	}

	public ShopEntry createEntry(UUID uuid, Shop shop, ItemStack displayItem, int slot) {
		return new BaseEntry(uuid, shop, displayItem, null, slot);
	}

	@Override
	public Map<UUID, ShopEntry> loadEntries(Shop shop) {
		return new HashMap<>();
	}

	@Override
	public void saveEntry(ShopEntry shopEntry) {

	}

	@Override
	public void deleteEntry(ShopEntry shopEntry) {

	}

	public Discount createDiscount(String nameFormat, SortedSet<LocalDateTime> start, Duration duration, double percent, String... tags) {
		return new Discount(UUID.randomUUID(), nameFormat, start, duration, percent, null, tags);
	}

	@Override
	public Map<UUID, Discount> loadDiscounts() {
		Map<UUID, Discount> map = new HashMap<>();
		Discount d1 = new Discount(UUID.randomUUID(), "XMas Discount", LocalDateTime.now(), Duration.of(3, ChronoUnit.DAYS), 80, null);
		//d1.addTag("global");
		map.put(d1.getUuid(), d1);
		return map;
	}

	@Override
	public void saveDiscount(Discount discount) {

	}

	@Override
	public void deleteDiscount(Discount discount) {

	}

	public Limit createLimit(String name) {
		return new Limit(name, Duration.of(3, ChronoUnit.DAYS), customer -> true, 32);
	}

	@Override
	public Map<UUID, Limit> loadLimits() {
		Map<UUID, Limit> map = new HashMap<>();
		Limit limit = createLimit("Example Limit");
		limit.addTag("global");
		Limit limit2 = createLimit("Example Limit");
		limit2.setGlobal(true);
		limit2.addTag("global");
		map.put(limit.getUuid(), limit);
		map.put(limit2.getUuid(), limit2);
		return map;
	}

	@Override
	public void saveLimit(Limit limit) {

	}

	@Override
	public void deleteLimit(Limit limit) {

	}

	public EntryTemplate createTemplate(String name) {
		return new EntryTemplate(UUID.randomUUID(), "<aqua>new-template");
	}

	@Override
	public Map<UUID, EntryTemplate> loadTemplates() {
		EntryTemplate template = new EntryTemplate(UUID.randomUUID(), "<gradient:dark_green:green:dark_green>Default Template");
		for (int i = 0; i < 9; i++) {
			int finalI = i;
			template.put("(<rows> - 1) * 9 + " + finalI, new BaseEntry(UUID.randomUUID(), null, ItemStackUtils.createItemStack(Material.DIAMOND,
					"lol", ""), null, i));
		}
		Map<UUID, EntryTemplate> map = new LinkedHashMap<>();
		map.put(template.getUuid(), template);
		return map;
	}

	@Override
	public void saveTemplate(EntryTemplate template) {

	}

	@Override
	public void deleteTemplate(EntryTemplate template) {

	}

	@Override
	public void logToDatabase(LogEntry entry) {

	}
}
