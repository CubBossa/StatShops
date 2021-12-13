package de.bossascrew.shops.general.menu;

import de.bossascrew.shops.general.util.Editable;
import org.jetbrains.annotations.Nullable;

public interface EditorMenu<P> {

	boolean isEditorSet();

	boolean setEditor(@Nullable P editor);

	P getEditor();
}
