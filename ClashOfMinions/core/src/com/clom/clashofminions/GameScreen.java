package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;
import com.badlogic.gdx.graphics.g2d.Sprite;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.Touchable;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.scenes.scene2d.utils.Drawable;
import com.badlogic.gdx.utils.Align;
import com.badlogic.gdx.utils.Array;
import com.clom.clashofminions.BattleField.BattleField;
import com.clom.clashofminions.Nodes.ButtonNode;
import com.clom.clashofminions.Nodes.ManaBarNode;
import com.clom.clashofminions.Nodes.SliderNode;
import com.clom.clashofminions.Nodes.SliderType;

import java.awt.Button;

/**
 * Created by greensn on 08.11.17.
 */

public class GameScreen implements Screen {

    final ClashOfMinions game;

    Stage stage;
    ButtonNode pauseButton;
    ButtonNode turnButton;

    Texture backgroundTexture;
    Texture pauseBackgroundTexture;
    Texture pauseButtonTexture;
    Texture turnButtonTexture;
    Texture quitButtonTexture;
    Texture continueButtonTexture;

    Group sliderGroup;
    Array<SliderNode> sliders = new Array<SliderNode>();

    ManaBarNode manaBarNode;

    BattleField battleField;

    Group pauseMenuGroup;

    GameScreen(final ClashOfMinions game)
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
        pauseButton = new ButtonNode(pauseButtonTexture);
        pauseButton.setHeight(UIConstants.pauseButtonWidth * stage.getWidth());
        pauseButton.setWidth(UIConstants.pauseButtonWidth * stage.getWidth());
        pauseButton.setPosition(UIConstants.pauseButtonPositionX * stage.getWidth(), UIConstants.pauseButtonPositionY * stage.getHeight());

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
        stage.addActor(pauseButton);
        stage.addActor(turnButton);

        setupSliders();
        setupPauseMenu();

        registerListeners();
    }

    void loadTextures()
    {
        backgroundTexture = new Texture(Gdx.files.internal("BattlefieldBackground.png"));
        pauseBackgroundTexture = new Texture(Gdx.files.internal("PauseMenu-Background.png"));
        pauseButtonTexture = new Texture(Gdx.files.internal("Button-Menu-Pause.png"));
        turnButtonTexture = new Texture(Gdx.files.internal("Button-Menu-Turn.png"));
        quitButtonTexture = new Texture(Gdx.files.internal("Button-Menu-Quit.png"));
        continueButtonTexture = new Texture(Gdx.files.internal("Button-Menu-Play.png"));
    }

    void registerListeners()
    {
        pauseButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Pause");
                pauseAction();
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

        Image nameTagSprite = new Image(pauseBackgroundTexture);
        nameTagSprite.setWidth(UIConstants.nameTagWidth * stage.getWidth());
        nameTagSprite.setHeight(UIConstants.nameTagHeight * stage.getWidth());
        nameTagSprite.setPosition(battleField.getX() + battleField.getWidth() * 0.125f, battleField.getY() - battleField.getHeight() * 0.15f);
        stage.addActor(nameTagSprite);

        Image nameTagSprite2 = new Image(pauseBackgroundTexture);
        nameTagSprite2.setWidth(UIConstants.nameTagWidth * stage.getWidth());
        nameTagSprite2.setHeight(UIConstants.nameTagHeight * stage.getWidth());
        nameTagSprite2.setPosition(battleField.getX() + battleField.getWidth() * (1 - 0.34f), battleField.getY() + battleField.getHeight() * (1 - 0.02f));
        stage.addActor(nameTagSprite2);

        Preferences preferences = Gdx.app.getPreferences("UserData");
        Label playerNameLabel = new Label(preferences.getString("userName"), game.skin);
        final GlyphLayout nameLayout = new GlyphLayout(UIConstants.font, playerNameLabel.getText());
        playerNameLabel.setPosition(nameTagSprite.getX() + nameTagSprite.getWidth()/2 - nameLayout.width/2, nameTagSprite.getY() + nameTagSprite.getHeight()/2 - nameLayout.height);
        stage.addActor(playerNameLabel);

        Label playerNameLabel2 = new Label("Mallory", game.skin);
        final GlyphLayout nameLayout2 = new GlyphLayout(UIConstants.font, playerNameLabel2.getText());
        playerNameLabel2.setPosition(nameTagSprite2.getX() + nameTagSprite2.getWidth()/2 - nameLayout2.width/2, nameTagSprite2.getY() + nameTagSprite2.getHeight()/2 - nameLayout2.height);
        stage.addActor(playerNameLabel2);
    }

    void setupPauseMenu()
    {
        pauseMenuGroup = new Group();
        pauseMenuGroup.setWidth(stage.getWidth());
        pauseMenuGroup.setHeight(stage.getHeight());
        pauseMenuGroup.setPosition(0, 0);
        stage.addActor(pauseMenuGroup);

        Image bgSprite = new Image(pauseBackgroundTexture);
        bgSprite.setWidth(pauseMenuGroup.getWidth());
        bgSprite.setHeight(pauseMenuGroup.getHeight());

        Table table = new Table();
        table.setWidth(stage.getWidth());
        table.setHeight(stage.getHeight());
        table.setPosition(0, 0);

        ButtonNode continueButton = new ButtonNode(continueButtonTexture);
        continueButton.setWidth(UIConstants.menuButtonWidth * stage.getHeight());
        continueButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());

        ButtonNode quitButton = new ButtonNode(quitButtonTexture);
        quitButton.setWidth(UIConstants.menuButtonWidth * stage.getHeight());
        quitButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());

        continueButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Continue");
                resumeAction();
            }
        });

        quitButton.addListener(new ClickListener()
        {
            @Override
            public void clicked(InputEvent event, float x, float y)
            {
                System.out.println("Quit");
                quitAction();
            }
        });


        table.add(continueButton).padBottom(100);
        table.row();
        table.add(quitButton);

        pauseMenuGroup.addActor(bgSprite);
        pauseMenuGroup.addActor(table);
        stage.addActor(pauseMenuGroup);

        pauseMenuGroup.setVisible(false);
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

    Boolean paused = false;

    private void pauseAction() {
        if (!paused)
        {
            paused = true;
            battleField.setTouchable(Touchable.disabled);
            sliderGroup.setTouchable(Touchable.disabled);
            turnButton.setTouchable(Touchable.disabled);

            pauseMenuGroup.setVisible(true);
        }
        else
        {
            resumeAction();
        }
    }

    private void resumeAction() {
        if (paused)
        {
            paused = false;
            battleField.setTouchable(Touchable.enabled);
            sliderGroup.setTouchable(Touchable.enabled);
            turnButton.setTouchable(Touchable.enabled);

            pauseMenuGroup.setVisible(false);
        }
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
