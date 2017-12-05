package com.clom.clashofminions;

import com.badlogic.gdx.ApplicationAdapter;
import com.badlogic.gdx.Game;
import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Camera;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.scenes.scene2d.ui.Skin;

public class ClashOfMinions extends Game {

	public static final float HEIGHT = 640;
	public static final float WIDTH = 1138;

	SpriteBatch batch;
	public Skin skin;

	Camera cam;

	@Override
	public void create () {
		batch = new SpriteBatch();
		UIConstants.font = new BitmapFont();

		skin = new Skin(Gdx.files.internal("visui_skin/uiskin.json"));

//		cam = new OrthographicCamera();
//		cam.viewportHeight = HEIGHT;
//		cam.viewportWidth = WIDTH;

		setScreen(new MainMenuScreen(this));
	}

	@Override
	public void render () {
		super.render();
	}

	@Override
	public void dispose () {
		batch.dispose();
	}
}