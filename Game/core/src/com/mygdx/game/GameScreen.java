package com.mygdx.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Button;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.badlogic.gdx.scenes.scene2d.ui.ImageButton;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.ui.TextButton;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.scenes.scene2d.utils.Drawable;
import com.badlogic.gdx.utils.Array;
import com.mygdx.game.BattleField.BattleField;
import com.mygdx.game.Nodes.ButtonNode;
import com.mygdx.game.Nodes.ManaBarNode;
import com.mygdx.game.Nodes.SliderNode;

/**
 * Created by greensn on 08.11.17.
 */

public class GameScreen implements Screen {

    final CardGame game;

    Stage stage;
    ButtonNode quitButton;
    ButtonNode turnButton;

    Texture backgroundTexture;

    Group sliderGroup;
    Array<SliderNode> sliders = new Array<SliderNode>();

    ManaBarNode manaBarNode;

    BattleField battleField;

    GameScreen(final CardGame game)
    {
        this.game = game;
        stage = new Stage();

        setupInterface();
        setupBattleField();

        Gdx.input.setInputProcessor(stage);
    }

    void setupInterface()
    {
        loadTextures();

        quitButton = new ButtonNode(backgroundTexture);
        quitButton.setHeight(0.075f * stage.getHeight());
        quitButton.setWidth(quitButton.getHeight());
        quitButton.setPosition(stage.getWidth() - quitButton.getWidth() - 10, stage.getHeight() - quitButton.getHeight() - 10);

        turnButton = new ButtonNode(backgroundTexture);
        turnButton.setPosition((1 - (0.1f + 1f/30f)) * stage.getWidth(), 0.03f * stage.getHeight());
        turnButton.setWidth(0.1f * stage.getWidth());
        turnButton.setHeight(0.36f * stage.getHeight());

        manaBarNode = new ManaBarNode(100);
        manaBarNode.setPosition(UIConstants.manaBarPositionX * stage.getWidth(), UIConstants.gameLowerPadding * stage.getHeight());
        manaBarNode.setWidth(UIConstants.manaBarWidth * stage.getWidth());
        manaBarNode.setHeight(UIConstants.manaBarHeight * stage.getHeight());
        manaBarNode.actualStep = 100;

        stage.addActor(manaBarNode);
        stage.addActor(quitButton);
        stage.addActor(turnButton);

        setupSliders();

        registerListeners();
    }

    void loadTextures()
    {
        backgroundTexture = new Texture(Gdx.files.internal("BattlefieldBackground.png"));
    }

    void registerListeners()
    {
        quitButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Quit");
                quitAction();
            }
        });

        turnButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Turn");
            }
        });
    }

    void setupSliders()
    {
        sliderGroup = new Group();
        sliderGroup.setWidth(UIConstants.sliderGroupWidth * stage.getWidth());
        sliderGroup.setHeight(UIConstants.sliderGroupHeight * stage.getHeight());
        sliderGroup.setPosition(1f/30f * stage.getWidth(), 0.03f * stage.getHeight());
        stage.addActor(sliderGroup);

        for (int i = 0; i < 8; i++)
        {
            SliderNode slider = new SliderNode(20, manaBarNode);
            slider.setPosition((0.04f + (0.12f * (float)i)) * sliderGroup.getWidth(), 0);
            slider.setWidth(0.08f * sliderGroup.getWidth());
            slider.setHeight(sliderGroup.getHeight());
            sliderGroup.addActor(slider);
            sliders.add(slider);
        }

    }

    void setupBattleField()
    {
        battleField = new BattleField(this);
        battleField.setHeight(UIConstants.battleFieldHeight * stage.getHeight());
        battleField.setWidth(battleField.getHeight()/UIConstants.battleFieldTilesVertical * UIConstants.battleFieldTilesHorizontal);
        battleField.setPosition((stage.getWidth()-battleField.getWidth())/2, UIConstants.battleFieldPositionY * stage.getHeight());
        stage.addActor(battleField);
        battleField.setup();
    }

    @Override
    public void show() {

    }

    @Override
    public void render(float delta) {
        Gdx.gl.glClearColor(0, 0, 1, 1);
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
        backgroundTexture.dispose();
    }

    private void quitAction()
    {
        game.setScreen(new MainMenuScreen(game));
    }
}
