package com.mygdx.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.utils.Array;
import com.mygdx.game.BattleField.BattleField;
import com.mygdx.game.Nodes.ButtonNode;
import com.mygdx.game.Nodes.ManaBarNode;
import com.mygdx.game.Nodes.SliderNode;
import com.mygdx.game.Nodes.SliderType;

/**
 * Created by greensn on 08.11.17.
 */

public class GameScreen implements Screen {

    final CardGame game;

    Stage stage;
    ButtonNode quitButton;
    ButtonNode turnButton;

    Texture backgroundTexture;
    Texture pauseButtonTexture;
    Texture turnButtonTexture;

    Group sliderGroup;
    Array<SliderNode> sliders = new Array<SliderNode>();

    ManaBarNode manaBarNode;

    BattleField battleField;

    GameScreen(final CardGame game)
    {
        this.game = game;
        stage = new Stage();

        loadTextures();
        setupBackground();
        setupBattleField();
        setupInterface();

        Gdx.input.setInputProcessor(stage);
    }

    void setupBackground()
    {
        GameBackgroundNode bg = new GameBackgroundNode();
        bg.setWidth(stage.getWidth());
        bg.setHeight(stage.getHeight());
        stage.addActor(bg);

        stage.addActor(bg);
    }

    void setupInterface()
    {
        quitButton = new ButtonNode(pauseButtonTexture);
        quitButton.setHeight(UIConstants.pauseButtonWidth * stage.getWidth());
        quitButton.setWidth(UIConstants.pauseButtonWidth * stage.getWidth());
        quitButton.setPosition(UIConstants.pauseButtonPositionX * stage.getWidth(), UIConstants.pauseButtonPositionY * stage.getHeight());

        turnButton = new ButtonNode(turnButtonTexture);
        turnButton.setPosition(UIConstants.turnButtonPositionX * stage.getWidth(), UIConstants.turnButtonPositionY * stage.getHeight());
        turnButton.setWidth(UIConstants.turnButtonWidth * stage.getWidth());
        turnButton.setHeight(UIConstants.turnButtonWidth * stage.getWidth() * 2);

        manaBarNode = new ManaBarNode(5);
        manaBarNode.setPosition(UIConstants.manaBarPositionX * stage.getWidth(), UIConstants.gameLowerPadding * stage.getHeight());
        manaBarNode.setWidth(UIConstants.manaBarWidth * stage.getWidth());
        manaBarNode.setHeight(UIConstants.manaBarHeight * stage.getHeight());
        manaBarNode.actualStep = 5;

        stage.addActor(manaBarNode);
        stage.addActor(quitButton);
        stage.addActor(turnButton);

        setupSliders();

        registerListeners();
    }

    void loadTextures()
    {
        backgroundTexture = new Texture(Gdx.files.internal("BattlefieldBackground.png"));
        pauseButtonTexture = new Texture(Gdx.files.internal("Button-Menu-Pause.png"));
        turnButtonTexture = new Texture(Gdx.files.internal("Button-Menu-Turn.png"));

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
                placeAction();
            }
        });
    }

    void setupSliders()
    {
        sliderGroup = new Group();
        sliderGroup.setWidth(UIConstants.sliderGroupWidth * stage.getWidth());
        sliderGroup.setHeight(UIConstants.sliderGroupHeight * stage.getHeight());
        sliderGroup.setPosition(UIConstants.sliderGroupPositionX * stage.getWidth(), UIConstants.gameLowerPadding * stage.getHeight());
        stage.addActor(sliderGroup);

        for (int i = 0; i < SliderType.numberOfTypes; i++)
        {
            SliderNode slider = new SliderNode(SliderType.values()[i], manaBarNode);
            slider.setPosition((((UIConstants.sliderPadding + UIConstants.sliderWidth) * (float)i)) * sliderGroup.getWidth(), 0);
            slider.setWidth(UIConstants.sliderWidth * sliderGroup.getWidth());
            slider.setHeight(sliderGroup.getHeight());
            slider.gameScreen = this;
            sliderGroup.addActor(slider);
            sliders.add(slider);
            slider.setup();
        }

    }

    void setupBattleField()
    {
        battleField = new BattleField(this);
        battleField.setHeight(UIConstants.battleFieldHeight * stage.getHeight());
        battleField.setWidth(battleField.getHeight()/UIConstants.battleFieldTilesVertical * UIConstants.battleFieldTilesHorizontal);
        battleField.setPosition(UIConstants.battleFieldPositionX * stage.getWidth(), UIConstants.battleFieldPositionY * stage.getHeight());
        stage.addActor(battleField);
        battleField.setup();
    }

    @Override
    public void show() {

    }

    @Override
    public void render(float delta) {
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT | (Gdx.graphics.getBufferFormat().coverageSampling?GL20.GL_COVERAGE_BUFFER_BIT_NV:0));

        stage.act(delta);
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

    private void placeAction()
    {
        updateMinionStats();
        battleField.placeFloatingMinion();
    }

    public void updateMinionStats()
    {
        if (battleField.floatingMinion != null)
        {
            for (SliderNode slider:sliders)
            {
                battleField.floatingMinion.minion.setAttribute(slider.type.toString(), slider.getSliderValue());
            }
            battleField.floatingMinion.minion.setAttribute("MaxHealth", battleField.floatingMinion.minion.getAttribute("Health"));
            battleField.floatingMinion.updateStats();
        }
    }

    public void gameOver()
    {
        Boolean leftWinner = battleField.battleFieldLogic.isLeftPlayerTurn;
        quitAction();
    }
}
