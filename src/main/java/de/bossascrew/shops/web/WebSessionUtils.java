package de.bossascrew.shops.web;

import com.fasterxml.jackson.databind.ObjectMapper;
import de.bossascrew.shops.web.pasting.ByteBinIntegration;
import de.bossascrew.shops.web.pasting.Paste;
import de.bossascrew.shops.web.pasting.PasteServer;
import lombok.experimental.UtilityClass;

@UtilityClass
public class WebSessionUtils {
    public static Paste generateWebSession(){

        WebJsonData data = new WebJsonData();

        ObjectMapper mapper = new ObjectMapper();
        try{
            String jsonString = mapper.writeValueAsString(data);
            PasteServer server = new ByteBinIntegration();
            return server.createPaste(jsonString);
        }catch(Exception e){
            e.printStackTrace();
        }

        return null;
    }
}
