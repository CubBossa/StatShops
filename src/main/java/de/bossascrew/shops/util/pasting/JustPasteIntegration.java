package de.bossascrew.shops.util.pasting;

import de.bossascrew.shops.util.justpasteit.JustPasteAPI;
import okhttp3.*;
import org.json.JSONObject;

import java.io.IOException;

public class JustPasteIntegration implements PasteServer {
    private final String hostname = "https://just-paste.it";

    @Override
    public Paste createPaste(String content) {
        String pasteURL = this.hostname + "/documents";
        try {
            MediaType PLAIN = MediaType.get("text/plain; charset=utf-8");
            OkHttpClient client = new OkHttpClient();
            Request request = new Request.Builder()
                    .url(pasteURL)
                    .post(RequestBody.create(PLAIN, content))
                    .build();
            Response response = client.newCall(request).execute();
            JSONObject jsonDocument = new JSONObject(response.body().string());

            Paste paste = new Paste(jsonDocument.getString("key"), hostname + "/" + jsonDocument.getString("key"), content, new Runnable() {
                @Override
                public void run() {
                    new JustPasteIntegration().deletePaste(jsonDocument.getString("key"), jsonDocument.get("deleteSecret") != null ? (jsonDocument.getString("deleteSecret")) : "");
                }
            });

            return paste;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public String getPasteContent(String id) {
        String pasteURL = hostname + "/documents/"+ id;
        try {
            OkHttpClient client = new OkHttpClient();
            Request request = new Request.Builder()
                    .url(pasteURL)
                    .build();
            Response response = client.newCall(request).execute();
            return new JSONObject(response.body().string()).getString("text");
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public boolean deletePaste(String id, String deleteKey) {
        String pasteURL = this.hostname + "/documents/delete/"+ id + "/" + deleteKey;
        try {
            OkHttpClient client = new OkHttpClient();
            Request request = new Request.Builder()
                    .url(pasteURL)
                    .build();
            Response response = client.newCall(request).execute();
            return response.code() == 200;
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        }
    }
}
