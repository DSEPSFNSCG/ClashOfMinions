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

/**
 * Created by greensn on 08.11.17.
 */

public class GameScreen implements Screen {

    final CardGame game;

    Stage stage;

    Texture backgroundTexture;

    GameScreen(final CardGame game)
    {
        this.game = game;

        stage = new Stage();

        TextButton quitButton = new TextButton("Menu", game.skin);

        stage.addActor(quitButton);

        quitButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Quit");
                quitAction();
            }
        });

        Gdx.input.setInputProcessor(stage);

        loadTextures();
    }

    void loadTextures()
    {
        backgroundTexture = new Texture(Gdx.files.internal("BattlefieldBackground.png"));
    }

    @Override
    public void show() {

    }

    @Override
    public void render(float delta) {
        Gdx.gl.glClearColor(0, 0, 1, 1);
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);

        game.batch.begin();
        game.batch.draw(backgroundTexture, 0.1f/3f * CardGame.WIDTH, 0.42f * CardGame.HEIGHT, (0.9f + 0.1f/3f) * CardGame.WIDTH, 0.5f * CardGame.HEIGHT);
        game.batch.end();

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
        backgroundTexture.dispose();
    }

    private void quitAction()
    {
        game.setScreen(new MainMenuScreen(game));
    }
}
