package de.bossascrew.shops.web;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.bossascrew.shops.ShopPlugin;
import de.bossascrew.shops.web.justpasteit.JustPasteAPI;
import de.bossascrew.shops.util.justpasteit.JustPasteAPI;
import de.bossascrew.shops.util.pasting.ByteBinIntegration;
import de.bossascrew.shops.util.pasting.Paste;
import de.bossascrew.shops.util.pasting.PasteServer;
import lombok.experimental.UtilityClass;

@UtilityClass
public class WebSessionUtils {
    public static String generateWebSession(){
        JustPasteAPI pasteAPI = new JustPasteAPI();

        WebJsonData data = new WebJsonData();

        ObjectMapper mapper = new ObjectMapper();
        try{
            String jsonString = mapper.writeValueAsString(data);
            PasteServer server = new ByteBinIntegration();
            Paste paste = server.createPaste(jsonString);
            return "https://localhost:8080/" + paste.getId();
        }catch(Exception e){
            e.printStackTrace();
        }

        return "error";
    }
}
