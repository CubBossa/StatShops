package de.bossascrew.shops.statshops.convertion;

import com.google.common.collect.Lists;
import de.bossascrew.shops.statshops.api.Shop;
import de.bossascrew.shops.statshops.api.ShopEntry;
import de.bossascrew.shops.statshops.handler.TemplateHandler;
import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.util.TagUtils;
import de.bossascrew.shops.statshops.StatShops;
import de.bossascrew.shops.statshops.data.FlatFileDatabase;
import de.bossascrew.shops.statshops.handler.DiscountHandler;
import de.bossascrew.shops.statshops.handler.LimitsHandler;
import de.bossascrew.shops.statshops.handler.ShopHandler;
import de.bossascrew.shops.statshops.shop.Discount;
import de.bossascrew.shops.statshops.shop.EntryTemplate;
import de.bossascrew.shops.statshops.shop.Limit;
import lombok.Getter;
import lombok.Setter;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * Converts template files to load pre-made shops from other users.
 */
@Getter
@Setter
public class DataPreset {

	public static final String FILE_ENDING = ".stat";

	public static final String ARG_SHOPS = "exclude_shops";
	public static final String ARG_ENTRIES = "exclude_entries";
	public static final String ARG_DISCOUNTS = "exclude_discounts";
	public static final String ARG_LIMITS = "exclude_limits";
	public static final String ARG_TEMPLATES = "exclude_templates";

	private final String name;
	private String author = "unknown";
	private String pluginVersion = "unknown";
	private LocalDateTime creation = LocalDateTime.MIN;
	private List<String> tags;

	private final Collection<Shop> shops = new HashSet<>();
	private final Collection<ShopEntry> entries = new HashSet<>();
	private final Collection<Discount> discounts = new HashSet<>();
	private final Collection<Limit> limits = new HashSet<>();
	private final Collection<EntryTemplate> templates = new HashSet<>();

	public DataPreset(File zipFile) {
		if (!zipFile.getName().endsWith(FILE_ENDING)) {
			throw new IllegalArgumentException("A template file must end with \"<name>" + FILE_ENDING + "\"");
		}
		this.name = zipFile.getName().replace(FILE_ENDING, "");
		try {
			loadFromZipFile(zipFile);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public DataPreset(String name, String author, String pluginVersion, LocalDateTime creation) {
		this.name = name;
		this.author = author;
		this.pluginVersion = pluginVersion;
		this.creation = creation;
	}

	public void loadFromCacheByTags(String... args) {
		List<String> argList = Lists.newArrayList(args);
		boolean includeShops = !argList.contains(ARG_SHOPS);
		boolean includeEntries = !argList.contains(ARG_ENTRIES);
		boolean includeDiscounts = !argList.contains(ARG_DISCOUNTS);
		boolean includeLimits = !argList.contains(ARG_LIMITS);
		boolean includeTemplates = !argList.contains(ARG_TEMPLATES);

		List<String> tags = argList.stream().filter(string -> !string.startsWith("exclude_")).collect(Collectors.toList());
		boolean noTags = tags.isEmpty();

		if (includeShops) {
			shops.clear();
			shops.addAll(ShopHandler.getInstance().getShops().stream().filter(shop -> noTags || TagUtils.hasCommonTags(shop, tags)).collect(Collectors.toList()));
		}
		if (includeEntries) {
			entries.clear();
			entries.addAll(shops.stream().flatMap(shop -> shop.getEntries().values().stream().filter(discount -> noTags || TagUtils.hasCommonTags(discount, tags))).collect(Collectors.toList()));
		}
		if (includeDiscounts) {
			discounts.clear();
			discounts.addAll(DiscountHandler.getInstance().getDiscounts().stream().filter(discount -> noTags || TagUtils.hasCommonTags(discount, tags)).collect(Collectors.toList()));
		}
		if (includeLimits) {
			limits.clear();
			limits.addAll(LimitsHandler.getInstance().getLimits().stream().filter(limit -> noTags || TagUtils.hasCommonTags(limit, tags)).collect(Collectors.toList()));
		}
		if (includeTemplates) {
			templates.clear();
			templates.addAll(TemplateHandler.getInstance().getTemplates());
		}
	}

	public void toFile() {
		StatShops.getInstance().log(LoggingPolicy.INFO, "Creating Data Template: " + name);

		File presetsDirectory = new File(StatShops.getInstance().getDataFolder(), "presets/");
		File directory = new File(presetsDirectory, name);
		directory.mkdirs();
		FlatFileDatabase database = new FlatFileDatabase(directory);

		shops.forEach(shop -> {
			database.saveShop(shop);
			shop.getEntries().forEach((integer, entry) -> database.saveEntry(entry));
		});
		discounts.forEach(database::saveDiscount);
		limits.forEach(database::saveLimit);
		templates.forEach(database::saveTemplate);

		try {
			File info = new File(directory, "info.yml");
			if (!info.exists()) {
				info.createNewFile();
			}
			YamlConfiguration infoCfg = YamlConfiguration.loadConfiguration(info);
			infoCfg.set("name", name);
			infoCfg.set("author", author);
			infoCfg.set("plugin-version", pluginVersion);
			infoCfg.set("creation-timestamp", creation.toString());
			infoCfg.set("tags", tags);
			infoCfg.save(info);


			FileOutputStream fileOut = new FileOutputStream(presetsDirectory.getAbsolutePath() + "\\" + name + FILE_ENDING);
			ZipOutputStream zipOut = new ZipOutputStream(fileOut);

			zipFile(database.getDirShops(), zipOut);
			zipFile(database.getDirDiscounts(), zipOut);
			zipFile(database.getDirLimits(), zipOut);
			zipFile(database.getDirTemplate(), zipOut);
			zipFile(info, zipOut);

			zipOut.close();
			fileOut.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		deleteFile(directory);

		StatShops.getInstance().log(LoggingPolicy.INFO, "Successfully created Data Template: " + name);
	}

	private void deleteFile(File file) {
		if (file.isDirectory()) {
			Arrays.stream(Objects.requireNonNull(file.listFiles())).forEach(this::deleteFile);
		}
		file.delete();
	}

	private void zipFile(File file, ZipOutputStream zipOut) throws IOException {
		zipFile(file, file.getName(), zipOut);
	}

	private void zipFile(File file, String fileName, ZipOutputStream zipOut) throws IOException {
		if (file.isHidden()) {
			return;
		}
		if (file.isDirectory()) {
			zipOut.putNextEntry(new ZipEntry(fileName + (fileName.endsWith("/") ? "" : "/")));
			zipOut.closeEntry();
			File[] children = file.listFiles();
			for (File childFile : children) {
				zipFile(childFile, fileName + "/" + childFile.getName(), zipOut);
			}
			return;
		}
		FileInputStream fis = new FileInputStream(file);
		ZipEntry zipEntry = new ZipEntry(fileName);
		zipOut.putNextEntry(zipEntry);
		byte[] bytes = new byte[1024];
		int length;
		while ((length = fis.read(bytes)) >= 0) {
			zipOut.write(bytes, 0, length);
		}
		fis.close();
	}

	private void loadFromZipFile(File zipFile) throws IOException {
		if (!zipFile.getName().endsWith(FILE_ENDING)) {
			StatShops.getInstance().log(LoggingPolicy.WARN, "Provided file is not a " + FILE_ENDING + "-file, errors might occur.");
		}
		String fileName = zipFile.getName();
		fileName = fileName.substring(0, fileName.lastIndexOf('.'));
		File presetsDirectory = new File(StatShops.getInstance().getDataFolder(), "presets/");
		File directory = new File(presetsDirectory, fileName);

		String fileZip = zipFile.getAbsolutePath();
		byte[] buffer = new byte[1024];
		ZipInputStream zis = new ZipInputStream(new FileInputStream(fileZip));
		ZipEntry zipEntry = zis.getNextEntry();
		while (zipEntry != null) {
			File newFile = newFile(directory, zipEntry);
			if (zipEntry.isDirectory()) {
				if (!newFile.isDirectory() && !newFile.mkdirs()) {
					throw new IOException("Failed to create directory " + newFile);
				}
			} else {
				File parent = newFile.getParentFile();
				if (!parent.isDirectory() && !parent.mkdirs()) {
					throw new IOException("Failed to create directory " + parent);
				}

				FileOutputStream fos = new FileOutputStream(newFile);
				int len;
				while ((len = zis.read(buffer)) > 0) {
					fos.write(buffer, 0, len);
				}
				fos.close();
			}
			zipEntry = zis.getNextEntry();
		}
		zis.closeEntry();
		zis.close();

		FlatFileDatabase database = new FlatFileDatabase(directory);
		this.shops.clear();
		this.shops.addAll(database.loadShops().values());
		this.discounts.clear();
		this.discounts.addAll(database.loadDiscounts().values());
		this.limits.clear();
		this.limits.addAll(database.loadLimits().values());
		this.templates.clear();
		this.templates.addAll(database.loadTemplates().values());
		deleteFile(directory);
	}

	public File newFile(File destinationDir, ZipEntry zipEntry) throws IOException {
		File destFile = new File(destinationDir, zipEntry.getName());

		String destDirPath = destinationDir.getCanonicalPath();
		String destFilePath = destFile.getCanonicalPath();

		if (!destFilePath.startsWith(destDirPath + File.separator)) {
			throw new IOException("Entry is outside of the target dir: " + zipEntry.getName());
		}

		return destFile;
	}

	public void apply(String... args) {
		List<String> argList = Lists.newArrayList(args);
		if (!argList.contains(ARG_TEMPLATES)) {
			templates.forEach(template -> TemplateHandler.getInstance().registerTemplate(template));
		}
		if (!argList.contains(ARG_DISCOUNTS)) {
			discounts.forEach(discount -> DiscountHandler.getInstance().addDiscount(discount));
		}
		if (!argList.contains(ARG_LIMITS)) {
			limits.forEach(limit -> LimitsHandler.getInstance().addLimit(limit));
		}
		if (!argList.contains(ARG_SHOPS)) {
			shops.forEach(shop -> ShopHandler.getInstance().addShop(shop));
		}
	}
}