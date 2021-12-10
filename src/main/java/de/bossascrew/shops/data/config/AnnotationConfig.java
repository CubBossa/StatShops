package de.bossascrew.shops.data.config;

import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.util.LoggingPolicy;
import org.bukkit.configuration.file.YamlConfiguration;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;

public class AnnotationConfig {

	private String path;

	public AnnotationConfig(String path) {
		this.path = path;
	}

	public boolean loadConfig() {
		return loadConfig(this.path);
	}

	public boolean loadConfig(String path) {
		ShopPlugin.getInstance().log(LoggingPolicy.INFO, "Loading config.yml with path: " + path);
		if (path == null) {
			path = "config.yml";
		}
		YamlConfiguration config = YamlConfiguration.loadConfiguration(new File(path));

		Class<? extends AnnotationConfig> cls = getClass();

		for (Field f : cls.getFields()) {
			if (f.isAnnotationPresent(ConfigEntry.class)) {
				String target = "";
				ConfigEntry cf = f.getAnnotation(ConfigEntry.class);
				target = cf.path();
				if (target.isEmpty()) {
					target = f.getName();
				}

				try {
					f.set(this, config.get(target, f.get(this)));
				} catch (IllegalArgumentException | IllegalAccessException e) {
					ShopPlugin.getInstance().log(LoggingPolicy.ERROR, "Could not load file " + path, e);
					return false;
				}
			}
		}
		return true;
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
