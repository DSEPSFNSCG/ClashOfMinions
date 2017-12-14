package com.clom.clashofminions;

import com.badlogic.gdx.ApplicationAdapter;
import com.badlogic.gdx.Game;
import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.files.FileHandle;
import com.badlogic.gdx.graphics.Camera;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Skin;

public class ClashOfMinions extends Game {

	public static final float HEIGHT = 640;
	public static final float WIDTH = 1138;

	SpriteBatch batch;
	//public Skin skin;

	Camera cam;

	@Override
	public void create () {
		batch = new SpriteBatch();
		//UIConstants.font = new BitmapFont();

		FileHandle xmlHandle = Gdx.files.internal("dimbo.fnt");
		FileHandle pngHandle = Gdx.files.internal("dimbo.png");
		UIConstants.font = new BitmapFont(xmlHandle, pngHandle, false);
		UIConstants.font.getRegion().getTexture().setFilter(Texture.TextureFilter.Linear, Texture.TextureFilter.Linear);

		UIConstants.labelStyle = new Label.LabelStyle(UIConstants.font, Color.WHITE);

		FileHandle xmlHandleSmall = Gdx.files.internal("dimboSmall.fnt");
		FileHandle pngHandleSmall = Gdx.files.internal("dimboSmall.png");
		UIConstants.fontSmall = new BitmapFont(xmlHandleSmall, pngHandleSmall, false);
		UIConstants.fontSmall.getRegion().getTexture().setFilter(Texture.TextureFilter.Linear, Texture.TextureFilter.Linear);

		UIConstants.labelStyleSmall = new Label.LabelStyle(UIConstants.fontSmall, Color.WHITE);

		//skin = new Skin(Gdx.files.internal("visui_skin/uiskin.json"));

		//cam = new OrthographicCamera();
		//cam.viewportHeight = HEIGHT;
		//cam.viewportWidth = WIDTH;
		//cam.update();

		Preferences preferences = Gdx.app.getPreferences("UserData");
		String name = preferences.getString("userName", "Bob");
		if (name.equals("Bob")) setScreen(new SettingsScreen(this));
		else setScreen(new MainMenuScreen(this));
	}

	@Override
	public void render () {

		//batch.setProjectionMatrix(cam.combined);

		super.render();
	}

	@Override
	public void dispose () {
		batch.dispose();
	}
}