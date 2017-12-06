package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.utils.Align;
import com.badlogic.gdx.utils.viewport.StretchViewport;
import com.clom.clashofminions.Connection.ConnectionHandler;
import com.clom.clashofminions.Connection.ConnectionHandlerDelegate;
import com.clom.clashofminions.Connection.AIConnectionHandler;
import com.clom.clashofminions.Nodes.ButtonNode;

/**
 * Created by greensn on 08.11.17.
 */

public class LoadingScreen implements Screen, ConnectionHandlerDelegate {

    final ClashOfMinions game;

    ConnectionHandler connectionHandler;

    Stage stage;
    Table table;

    float loadingTime = 0;

    LoadingScreen(final ClashOfMinions game)
    {
        this.game = game;

        stage = new Stage(new StretchViewport(ClashOfMinions.WIDTH, ClashOfMinions.HEIGHT));

        MenuBackgroundNode bg = new MenuBackgroundNode();
        bg.setWidth(stage.getWidth());
        bg.setHeight(stage.getHeight());
        stage.addActor(bg);


        Float labelScale = 0.65f;
        Label loadingLabel = new Label("Searching game...", UIConstants.labelStyle);
        loadingLabel.setFontScale(labelScale);
        loadingLabel.setBounds(0, stage.getHeight()*0.8f, stage.getWidth(), stage.getHeight()*0.2f);
        loadingLabel.setAlignment(Align.center);
        stage.addActor(loadingLabel);

        table = new Table();
        table.setWidth(stage.getWidth());
        table.setHeight(stage.getHeight()/4);
        table.setPosition(0, stage.getHeight()/2);
        stage.addActor(table);

        ButtonNode backButton = new ButtonNode(new Texture(Gdx.files.internal("Button-Menu-Back.png")));
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
        //if (loadingTime > 0.1) enterGame();
    }

    private void connectToServer()
    {
        System.out.println("Connecting...");
        Preferences preferences = Gdx.app.getPreferences("UserData");
        String name = preferences.getString("userName", "");
        Boolean gameRunning = preferences.getBoolean("gameRunning", false);

        connectionHandler = new AIConnectionHandler();
        connectionHandler.setDelegate(this);

        if (gameRunning)
        {
            String token = preferences.getString("gameToken", "");
            connectionHandler.restoreGame(token);
        }
        else
        {
            connectionHandler.searchGame(name);
        }
    }

    private void closeServerConnection()
    {
        System.out.println("Disconnect");

        Preferences preferences = Gdx.app.getPreferences("UserData");
        preferences.putBoolean("gameRunning", false);
        preferences.flush();

        connectionHandler.cancelSearchingGame();
        game.setScreen(new MainMenuScreen(game));
    }

    private void enterGame()
    {
        game.setScreen(new GameScreen(game, connectionHandler));
    }



    @Override
    public void gameFound(String token, String opponentName, Boolean isFirstPlayer) {
        Preferences preferences = Gdx.app.getPreferences("UserData");
        preferences.putBoolean("gameRunning", true);
        preferences.putBoolean("isFirstPlayer", isFirstPlayer);
        preferences.putString("gameToken", token);
        preferences.putString("opponentName", opponentName);
        preferences.flush();

        enterGame();
    }

    @Override
    public void receivedMove(int x, int y, int[] values) {

    }

    @Override
    public void opponentQuit() {

    }

    @Override
    public void restoredGame(int[] xs, int[] ys, int[][] valuesArray) {
        GameScreen gameScreen = new GameScreen(game, connectionHandler);
        gameScreen.restoredGame(xs, ys, valuesArray);
        game.setScreen(gameScreen);
    }
}
