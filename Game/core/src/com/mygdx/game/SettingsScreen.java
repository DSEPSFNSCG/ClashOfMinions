package com.mygdx.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.ui.TextButton;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.mygdx.game.Nodes.ButtonNode;

/**
 * Created by greensn on 08.11.17.
 */

public class SettingsScreen implements Screen {

    final CardGame game;

    Stage stage;
    Table table;

    SettingsScreen(final CardGame game)
    {
        this.game = game;

        stage = new Stage();

        table = new Table();
        table.setFillParent(true);
        //table.setDebug(true);
        stage.addActor(table);

        ButtonNode backButton = new ButtonNode(new Texture(Gdx.files.internal("BattlefieldBackground.png")));
        backButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());
        backButton.setWidth(UIConstants.menuButtonWidth * stage.getWidth());

        table.add(backButton);

        backButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Back");
                game.setScreen(new MainMenuScreen(game));
            }
        });

        Gdx.input.setInputProcessor(stage);
    }

    @Override
    public void show() {

    }

    @Override
    public void render(float delta) {
        Gdx.gl.glClearColor(0, 1, 0, 1);
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);

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
    }
}
