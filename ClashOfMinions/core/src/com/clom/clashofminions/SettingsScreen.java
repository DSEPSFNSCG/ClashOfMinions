package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.ui.TextField;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.clom.clashofminions.Nodes.ButtonNode;

/**
 * Created by greensn on 08.11.17.
 */

public class SettingsScreen implements Screen {

    final ClashOfMinions game;

    Stage stage;
    Table table;

    SettingsScreen(final ClashOfMinions game)
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

        TextField textField = new TextField("", game.skin);
        Label label = new Label("Your name:", game.skin);
        label.setColor(0, 0, 0, 1);

        table.add(label).padBottom(10);
        table.row();
        table.add(textField).padBottom(UIConstants.menuButtonHeight * stage.getHeight());;
        table.row();
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
