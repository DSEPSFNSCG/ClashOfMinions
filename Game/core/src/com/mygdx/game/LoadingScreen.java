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

public class LoadingScreen implements Screen {

    final CardGame game;

    Stage stage;
    Table table;

    float loadingTime = 0;

    LoadingScreen(final CardGame game)
    {
        this.game = game;

        stage = new Stage();

        MenuBackgroundNode bg = new MenuBackgroundNode();
        bg.setWidth(stage.getWidth());
        bg.setHeight(stage.getHeight());
        stage.addActor(bg);

        table = new Table();
        table.setFillParent(true);
        //table.setDebug(true);
        stage.addActor(table);

        ButtonNode backButton = new ButtonNode(new Texture(Gdx.files.internal("Button-Menu.png")));
        backButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());
        backButton.setWidth(UIConstants.menuButtonWidth * stage.getHeight());


        table.add(backButton);

        backButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Back");
                closeServerConnection();
            }
        });

        Gdx.input.setInputProcessor(stage);

        //Connect to server
        connectToServer();
    }

    @Override
    public void show() {

    }

    @Override
    public void render(float delta) {
        update(delta);

        Gdx.gl.glClearColor(0, 0, 0, 1);
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

    private void update(float delta)
    {
        loadingTime += delta;
        if (loadingTime > 0.1) gotServerResponse();
    }

    private void connectToServer()
    {
        System.out.println("Connecting...");
    }

    private void closeServerConnection()
    {
        System.out.println("Disconnect");
        game.setScreen(new MainMenuScreen(game));
    }

    void gotServerResponse()
    {
        enterGame();
    }

    private void enterGame()
    {
        game.setScreen(new GameScreen(game));
    }
}
