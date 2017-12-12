package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.utils.viewport.StretchViewport;
import com.clom.clashofminions.Nodes.ButtonNode;

/**
 * Created by greensn on 07.11.17.
 */

public class MainMenuScreen implements Screen {

    final ClashOfMinions game;

    ButtonNode playButton;

    Stage stage;
    Table table;

    MainMenuScreen(final ClashOfMinions game)
    {
        this.game = game;

        stage = new Stage(new StretchViewport(ClashOfMinions.WIDTH, ClashOfMinions.HEIGHT));


        MenuBackgroundNode bg = new MenuBackgroundNode();
        bg.setWidth(stage.getWidth());
        bg.setHeight(stage.getHeight());
        stage.addActor(bg);

        table = new Table();
        table.setWidth(stage.getWidth());
        table.setHeight(stage.getHeight()/4);
        table.setPosition(0, stage.getHeight()/2);
        stage.addActor(table);

        playButton = new ButtonNode(new Texture(Gdx.files.internal("Button-Menu-Play.png")));
        playButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());
        playButton.setWidth(UIConstants.menuButtonWidth * stage.getHeight());

        ButtonNode settingsButton = new ButtonNode(new Texture(Gdx.files.internal("Button-Menu-Settings.png")));
        settingsButton.setHeight(UIConstants.menuButtonSettingsSize * stage.getHeight());
        settingsButton.setWidth(UIConstants.menuButtonSettingsSize * stage.getHeight());
        settingsButton.setPosition(10f, stage.getHeight() - settingsButton.getHeight() - 10f);

        table.add(playButton);

        stage.addActor(settingsButton);

        playButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Play");
                Preferences preferences = Gdx.app.getPreferences("UserData");
                String name = preferences.getString("userName", "Bob");
                String address = preferences.getString("serverAddress", "infinisil.com:8081");
                game.setScreen(new LoadingScreen(game,address));
                dispose();
            }
        });

        settingsButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Settings");
                MainMenuScreen.this.game.setScreen(new SettingsScreen(game));
                dispose();
            }
        });

        Gdx.input.setInputProcessor(stage);
    }

    @Override
    public void show() {

    }

    @Override
    public void render(float delta) {
        Gdx.gl.glClearColor(1, 1, 1, 1);
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);

        //stage.act(Gdx.graphics.getDeltaTime());
        stage.draw();
    }

    @Override
    public void resize(int width, int height) {

    }

    @Override
    public void pause() {

    }

    @Override
    public void resume() {

    }

    @Override
    public void hide() {

    }

    @Override
    public void dispose() {
        stage.dispose();
        playButton.remove();
    }
}
