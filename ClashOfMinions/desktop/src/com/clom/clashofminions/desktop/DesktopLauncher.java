package com.clom.clashofminions.desktop;

import com.badlogic.gdx.backends.lwjgl.LwjglApplication;
import com.badlogic.gdx.backends.lwjgl.LwjglApplicationConfiguration;
import com.clom.clashofminions.ClashOfMinions;

public class DesktopLauncher {
	public static void main (String[] arg) {
		LwjglApplicationConfiguration config = new LwjglApplicationConfiguration();
		config.height = (int) ClashOfMinions.HEIGHT;
		config.width = (int) ClashOfMinions.WIDTH;
		config.resizable = false;
		config.samples = 3;
		//config.fullscreen = true;
		new LwjglApplication(new ClashOfMinions(), config);
	}
}
