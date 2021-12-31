package de.bossascrew.shops.general.util;

import com.mojang.authlib.GameProfile;
import lombok.experimental.UtilityClass;
import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.SkullMeta;
import org.jetbrains.annotations.Nullable;

import java.lang.reflect.Field;

@UtilityClass
public class YamlUtils {

	public static final OfflinePlayer STEVE = Bukkit.getOfflinePlayer("steve");

	public ItemStack loadSkull(ConfigurationSection section, String key) {
		ItemStack stack = section.getItemStack(key);
		String url = section.getString(key + "-skull-url");
		if (stack == null || url == null) {
			return stack;
		}
		if (stack.getItemMeta() instanceof SkullMeta meta) {
			meta.setOwningPlayer(null);
		} else {
			return stack;
		}
		return ItemStackUtils.createCustomHead(stack, url);
	}

	public void saveSkull(ConfigurationSection section, String key, @Nullable ItemStack stack) {
		if (stack == null) {
			section.set(key, stack);
			return;
		}
		ItemMeta itemMeta = stack.getItemMeta();
		if (itemMeta instanceof SkullMeta meta) {
			try {
				Field profileField = meta.getClass().getDeclaredField("profile");
				profileField.setAccessible(true);
				String url = ((GameProfile) profileField.get(meta)).getProperties().get("textures").iterator().next().getValue();

				meta.setOwningPlayer(STEVE);
				stack.setItemMeta(meta);
				section.set(key, stack);
				section.set(key + "-skull-url", url);

			} catch (IllegalArgumentException | NoSuchFieldException | SecurityException | IllegalAccessException error) {
				error.printStackTrace();
			}
		} else {
			section.set(key, stack);
		}
	}


}
