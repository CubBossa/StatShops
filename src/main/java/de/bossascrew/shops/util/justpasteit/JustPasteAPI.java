package de.bossascrew.shops.util.justpasteit;

import okhttp3.*;
import org.jetbrains.annotations.Nullable;
import org.json.JSONObject;

import java.io.IOException;
import java.util.Objects;

public class JustPasteAPI {

    private static String uri;

    public JustPasteAPI(String hostname){
        uri = hostname;
    }

    public JustPasteAPI(){
        uri = "https://just-paste.it";
    }

    public String getUri(){
        return uri;
    }

    public JustPasteAPIData createPaste(String content) {
        String pasteURL = uri + "/documents";
        try {
            MediaType PLAIN = MediaType.get("text/plain; charset=utf-8");
            OkHttpClient client = new OkHttpClient();
            Request request = new Request.Builder()
                    .url(pasteURL)
                    .post(RequestBody.create(PLAIN, content))
                    .build();
            Response response = client.newCall(request).execute();
            JSONObject jsonDocument = new JSONObject(response.body().string());
            JustPasteAPIData putils = new JustPasteAPIData();
            putils.setDeleteSecret(jsonDocument.get("deleteSecret") != null ? (jsonDocument.getString("deleteSecret")) : "");
            putils.setKey(jsonDocument.getString("key"));
            putils.setFormattedURL(uri + "/" + putils.getKey());
            return putils;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    public String getPaste(String key) {
        String pasteURL = uri + "/documents/"+key;
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

    public boolean deletePaste(String key, String deleteSecret) {
        String pasteURL = uri + "/documents/delete/"+ key + "/" + deleteSecret;
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

    public static class JustPasteAPIData {

        private String key = "";
        private String deleteSecret = "";
        private String formattedURL = "";

        private JustPasteAPIData(){ }

        public String getKey() {
            return key;
        }

        private void setKey(String key) {
            this.key = key;
        }

        private void setDeleteSecret(String deleteSecret) {
            this.deleteSecret = deleteSecret;
        }

        public String getDeleteSecret() {
            return deleteSecret;
        }

        private void setFormattedURL(String formattedURL) {
            this.formattedURL = formattedURL;
        }

        public String getFormattedURL() {
            return formattedURL;
        }

    }

}
