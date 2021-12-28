package de.bossascrew.shops.general.config;

import de.bossascrew.shops.general.util.LoggingPolicy;
import de.bossascrew.shops.statshops.StatShops;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class AnnotationConfig {

	private String path;

	public AnnotationConfig(String path) {
		this.path = path;
	}

	public boolean loadConfig() {
		return loadConfig(this.path);
	}

	public boolean loadConfig(String path) {
		boolean success = true;
		StatShops.getInstance().log(LoggingPolicy.INFO, "Loading config.yml with path: " + path);
		if (path == null) {
			path = "config.yml";
		}
		File file = new File(path);
		YamlConfiguration config = YamlConfiguration.loadConfiguration(file);

		Class<? extends AnnotationConfig> cls = getClass();

		boolean requiresSave = false;
		for (Field f : cls.getFields()) {
			if (f.isAnnotationPresent(ConfigEntry.class)) {
				String target;
				ConfigEntry cf = f.getAnnotation(ConfigEntry.class);
				target = cf.path();
				if (target.isEmpty()) {
					target = f.getName();
				}

				try {
					if (f.getType().isEnum()) {
						String input = config.getString(target);
						if (input != null) {
							Method valueOf = f.getType().getMethod("valueOf", String.class);
							f.set(this, valueOf.invoke(null, input));
						} else {
							config.set(cf.path(), f.get(this));
							requiresSave = true;
						}
					} else {
						var x = config.get(target);
						if (x == null) {
							config.set(cf.path(), f.get(this));
							requiresSave = true;
						} else {
							f.set(this, x);
						}
					}
				} catch (IllegalArgumentException | IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
					StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not load file " + path, e);
					success = false;
				}
			}
		}
		try {
			if (requiresSave) {
				config.save(file);
			}
		} catch (IOException e) {
			StatShops.getInstance().log(LoggingPolicy.ERROR, "Could not save new default values to config.yml", e);
			success = false;
		}
		return success;
	}

	public boolean saveConfig() {
		return saveConfig(this.path);
	}

	public boolean saveConfig(String path) {
		StatShops.getInstance().log(LoggingPolicy.INFO, "Saving config.yml with path: " + path);

		if (path == null || path.isEmpty()) {
			path = "config.yml";
		}
		this.path = path;

		YamlConfiguration config = new YamlConfiguration();
		String version = config.getString("version");

		Class<? extends AnnotationConfig> cls = getClass();

		for (Field f : cls.getFields()) {
			if (f.isAnnotationPresent(ConfigEntry.class)) {
				String target;
				ConfigEntry cf = f.getAnnotation(ConfigEntry.class);
				target = cf.path();
				if (target.isEmpty()) {
					target = f.getName();
				}
				try {
					config.set(target, f.get(this));
				} catch (IllegalArgumentException | IllegalAccessException e) {
					e.printStackTrace();
					return false;
				}
			}
		}
		try {
			config.save(new File(path));
			return true;
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
	}
}
