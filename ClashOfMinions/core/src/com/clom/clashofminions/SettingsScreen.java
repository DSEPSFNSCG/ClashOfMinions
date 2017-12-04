package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.ui.TextField;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.utils.Align;
import com.clom.clashofminions.Nodes.ButtonNode;

/**
 * Created by greensn on 08.11.17.
 */

public class SettingsScreen implements Screen {

    final ClashOfMinions game;
    Preferences preferences;

    Stage stage;
    Table table;

    SettingsScreen(final ClashOfMinions game)
    {
        this.game = game;

        //Preferences
        preferences = Gdx.app.getPreferences("UserData");
        String name = preferences.getString("userName", "");
        String address = preferences.getString("serverAddress", "infinisil.com:8081");

        stage = new Stage();

        //Background
        MenuBackgroundNode bg = new MenuBackgroundNode();
        bg.setWidth(stage.getWidth());
        bg.setHeight(stage.getHeight());
        stage.addActor(bg);

        //Sign
        Image sign = new Image(new Texture(Gdx.files.internal("Settings-Sign.png")));
        sign.setWidth(stage.getHeight() * 0.4f);
        sign.setHeight(stage.getHeight() * 0.4f);
        sign.setPosition(stage.getWidth()/2, stage.getHeight() * 0.64f, Align.center);
        stage.addActor(sign);

        //Table
        table = new Table();
        table.setFillParent(true);
        stage.addActor(table);

        //BackButton
        ButtonNode backButton = new ButtonNode(new Texture(Gdx.files.internal("Button-Menu-Back.png")));
        backButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());
        backButton.setWidth(UIConstants.menuButtonWidth * stage.getHeight());

        //Name
        final TextField nameTextField = new TextField(name, game.skin);
        nameTextField.setText(name);
        Label nameLabel = new Label("Your name:", game.skin);
        nameLabel.setColor(0, 0, 0, 1);

        //IP
        final TextField addressTextField = new TextField(address, game.skin);
        Label addressLabel = new Label("Server address:", game.skin);
        addressLabel.setColor(0, 0, 0, 1);

        table.add(nameLabel).padBottom(10);
        table.row();
        table.add(nameTextField).padBottom(UIConstants.menuButtonHeight * stage.getHeight() * 0.5f);;
        table.row();
        table.add(addressLabel).padBottom(10);
        table.row();
        table.add(addressTextField).padBottom(UIConstants.menuButtonHeight * stage.getHeight());;
        table.row();
        table.add(backButton);

        backButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Back");
                game.setScreen(new MainMenuScreen(game));
                preferences.putString("userName", nameTextField.getText());
                preferences.putString("serverAddress", addressTextField.getText());
                preferences.flush();
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
