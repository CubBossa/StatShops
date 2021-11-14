package de.bossascrew.shops.web.pasting;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class Paste {
    private String id;
    private String formattedURL;
    private String content;
    private Runnable deleteRunnable;
}
