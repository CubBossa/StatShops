package de.bossascrew.shops.data.config;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.util.LoggingPolicy;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.*;
import java.lang.reflect.Field;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AnnotationConfig {

	private String path;

	public AnnotationConfig(String path) {
		this.path = path;
	}

	public void loadConfig() {
		loadConfig(this.path);
	}

	public void loadConfig(String path) {
		ShopPlugin.getInstance().log(LoggingPolicy.INFO, "Loading config.yml with path: " + path);
		if (path == null) {
			path = "config.yml";
		}

		String strConfig = "";
		try {
			BufferedReader reader = new BufferedReader(new FileReader(path));
			String s;
			StringBuilder sb = new StringBuilder();
			while ((s = reader.readLine()) != null) {
				sb.append(s).append("\n");
			}
			reader.close();
			strConfig = Pattern.compile("\\r?\\n? *?#[^\\r\\n]*").matcher(sb.toString()).replaceAll("");
			while (strConfig.startsWith("\n")) {
				strConfig = strConfig.substring(1);
			}
			strConfig = strConfig.replaceAll("\n+", "\n");

		} catch (IOException ignored) {
			ignored.printStackTrace();
		}
		YamlConfiguration config = new YamlConfiguration();
		try {
			config.loadFromString(strConfig);
		} catch (InvalidConfigurationException ignored) {
			ignored.printStackTrace();
		}

		Class<? extends AnnotationConfig> cls = getClass();

		for (Field f : cls.getFields()) {
			if (f.isAnnotationPresent(ConfigEntry.class)) {
				String target = "";
				ConfigEntry cf = f.getAnnotation(ConfigEntry.class);
				target = cf.name();
				if (target.isEmpty()) {
					target = f.getName();
				}

				try {
					f.set(this, config.get(target, f.get(this)));
				} catch (IllegalArgumentException | IllegalAccessException ignored) {
					ignored.printStackTrace();
				}
			}
		}
	}

	public boolean saveConfig() {
		return saveConfig(this.path);
	}

	public boolean saveConfig(String path) {
		ShopPlugin.getInstance().log(LoggingPolicy.INFO, "Saving config.yml with path: " + path);

		if (path == null || path.isEmpty()) {
			path = "config.yml";
		}
		this.path = path;

		YamlConfiguration config = new YamlConfiguration();

		Class<? extends AnnotationConfig> cls = getClass();

		for (Field f : cls.getFields()) {
			if (f.isAnnotationPresent(ConfigEntry.class)) {
				String target;
				ConfigEntry cf = f.getAnnotation(ConfigEntry.class);
				target = cf.name();
				if (target.isEmpty()) {
					target = f.getName();
				}
				try {
					if (!cf.comment().isEmpty()) {
						config.set(target + "_COMMENT", cf.comment());
					}
					config.set(target, f.get(this));
				} catch (IllegalArgumentException | IllegalAccessException e) {
					e.printStackTrace();
					return false;
				}
			}
		}

		try {
			String configString = config.saveToString();

			if (getClass().isAnnotationPresent(ConfigFile.class)) {
				String header = getClass().getAnnotation(ConfigFile.class).header();
				StringBuilder headerS = new StringBuilder();
				if (!header.isEmpty()) {
					for (String s : header.split("\n")) {
						headerS.append("# ").append(s).append("\n");
					}
				}
				configString = headerS + configString;
			}

			Matcher matcher = Pattern.compile("(?:[A-Za-z0-9_]*?)_COMMENT: ?(.*?)(\\n[^:\\n]*?:)", Pattern.DOTALL).matcher(configString);

			StringBuilder newConfig = new StringBuilder();

			while (matcher.find()) {
				String comm = matcher.group(1);
				comm = "# " + Pattern.compile("\n( *)").matcher(comm).replaceAll("\n$1# ");
				comm += matcher.group(2);
				matcher.appendReplacement(newConfig, Matcher.quoteReplacement(comm));
			}
			matcher.appendTail(newConfig);
			configString = newConfig.toString();


			File f = new File(path).getParentFile();

			if (f != null) {
				f.mkdirs();
			}

			FileWriter file = new FileWriter(path);

			file.write(configString);

			file.close();
			return true;
		} catch (IOException exception) {
			exception.printStackTrace();
			return false;
		}
	}
}
