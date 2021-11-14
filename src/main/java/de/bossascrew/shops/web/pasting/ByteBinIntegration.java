package de.bossascrew.shops.web.pasting;

import okhttp3.*;
import org.json.JSONObject;

import java.io.IOException;

public class ByteBinIntegration implements PasteServer{
    private final String hostname = "https://bytebin.lucko.me";

    @Override
    public Paste createPaste(String content) {
        String pasteURL = this.hostname + "/post";
        try{
            OkHttpClient client = new OkHttpClient();
            MediaType PLAIN = MediaType.get("text/plain; charset=utf-8");
            Request request = new Request.Builder()
                    .url(pasteURL)
                    .post(RequestBody.create(PLAIN, content))
                    .addHeader("User-Agent", "Minecraft shop-plugin with webinterface by jannis6023.de")
                    .build();
            Response response = client.newCall(request).execute();
            JSONObject jsonDocument = new JSONObject(response.body().string());

            String key = jsonDocument.getString("key");
            Paste responsePaste = new Paste(key, hostname + "/" + key, content, null);

            return responsePaste;
        }catch (Exception e){
            return null;
        }
    }

    @Override
    public String getPasteContent(String id) {
        String pasteURL = hostname + "/"+ id;
        try {
            OkHttpClient client = new OkHttpClient();
            Request request = new Request.Builder()
                    .url(pasteURL)
                    .build();
            Response response = client.newCall(request).execute();
            return response.body().string();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public boolean deletePaste(String id, String deleteKey) {
        return false;
    }
}
