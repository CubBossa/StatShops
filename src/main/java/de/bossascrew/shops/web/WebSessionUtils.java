package de.bossascrew.shops.web;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.web.justpasteit.JustPasteAPI;
import lombok.experimental.UtilityClass;

@UtilityClass
public class WebSessionUtils {
    public static String generateWebSession(){
        JustPasteAPI pasteAPI = new JustPasteAPI();

        ObjectMapper mapper = new ObjectMapper();
        try{
            String jsonString = mapper.writeValueAsString(ShopPlugin.getInstance().getShopHandler().getShops());
            JustPasteAPI.JustPasteAPIData pasteData = pasteAPI.createPaste(jsonString);
            return pasteData.getFormattedURL();
        }catch(Exception e){
            e.printStackTrace();
        }

        return "error";
    }
}
