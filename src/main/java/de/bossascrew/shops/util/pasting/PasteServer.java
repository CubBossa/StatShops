package de.bossascrew.shops.util.pasting;

public interface PasteServer {
    Paste createPaste(String content);
    String getPasteContent(String id);
    boolean deletePaste(String id, String deleteKey);
}
