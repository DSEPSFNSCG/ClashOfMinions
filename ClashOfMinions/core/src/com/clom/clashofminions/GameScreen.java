package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.Touchable;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.utils.Align;
import com.badlogic.gdx.utils.Array;
import com.badlogic.gdx.utils.viewport.StretchViewport;
import com.clom.clashofminions.BattleField.BattleField;
import com.clom.clashofminions.BattleField.MinionNode;
import com.clom.clashofminions.Connection.ConnectionHandler;
import com.clom.clashofminions.Connection.ConnectionHandlerDelegate;
import com.clom.clashofminions.Nodes.ButtonNode;
import com.clom.clashofminions.Nodes.GameOverGroup;
import com.clom.clashofminions.Nodes.ManaBarNode;
import com.clom.clashofminions.Nodes.SliderNode;
import com.clom.clashofminions.Nodes.SliderType;


/**
 * Created by greensn on 08.11.17.
 */

public class GameScreen implements Screen, ConnectionHandlerDelegate {

    public final ClashOfMinions game;

    ButtonNode continueButton;
    ButtonNode quitButton;

    ConnectionHandler connectionHandler;

    Stage stage;
    ButtonNode pauseButton;
    ButtonNode turnButton;

    Texture backgroundTexture;
    Texture pauseBackgroundTexture;
    Texture pauseButtonTexture;
    Texture turnButtonTexture;
    Texture quitButtonTexture;
    Texture continueButtonTexture;
    Texture nameTagTexture;

    Group sliderGroup;
    Array<SliderNode> sliders = new Array<SliderNode>();

    ManaBarNode manaBarNode;

    BattleField battleField;

    Group pauseMenuGroup;

    Label turnLabel;

    Boolean isFirstPlayer;

    GameOverGroup gameOverGroup;

    GameScreen(final ClashOfMinions game, ConnectionHandler connectionHandler)
    {
        this.game = game;
        this.connectionHandler = connectionHandler;
        connectionHandler.setDelegate(this);

        stage = new Stage(new StretchViewport(ClashOfMinions.WIDTH, ClashOfMinions.HEIGHT));

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

        manaBarNode = new ManaBarNode(8);
        manaBarNode.setPosition(UIConstants.manaBarPositionX * stage.getWidth(), UIConstants.gameLowerPadding * stage.getHeight());
        manaBarNode.setWidth(UIConstants.manaBarWidth * stage.getWidth());
        manaBarNode.setHeight(UIConstants.manaBarHeight * stage.getHeight());
        manaBarNode.actualStep = 8;

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
        nameTagTexture = new Texture(Gdx.files.internal("NameSign.png"));
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
        Preferences preferences = Gdx.app.getPreferences("UserData");

        battleField = new BattleField(this);
        battleField.setHeight(UIConstants.battleFieldHeight * stage.getHeight());
        battleField.setWidth(battleField.getHeight()/UIConstants.battleFieldTilesVertical * UIConstants.battleFieldTilesHorizontal);
        battleField.setPosition(UIConstants.battleFieldPositionX * stage.getWidth(), UIConstants.battleFieldPositionY * stage.getHeight());
        stage.addActor(battleField);
        battleField.setup();
        isFirstPlayer = preferences.getBoolean("isFirstPlayer", true);
        battleField.battleFieldLogic.isLeftPlayerTurn = isFirstPlayer;

        Image nameTagSprite = new Image(nameTagTexture);
        nameTagSprite.setWidth(UIConstants.nameTagWidth * stage.getWidth());
        nameTagSprite.setHeight(UIConstants.nameTagHeight * stage.getWidth());
        nameTagSprite.setPosition(battleField.getX() + battleField.getWidth() * 0.025f, battleField.getY() + battleField.getHeight() * (1 - 0.0f));
        stage.addActor(nameTagSprite);

        Image nameTagSprite2 = new Image(nameTagTexture);
        nameTagSprite2.setWidth(UIConstants.nameTagWidth * stage.getWidth());
        nameTagSprite2.setHeight(UIConstants.nameTagHeight * stage.getWidth());
        nameTagSprite2.setPosition(battleField.getX() + battleField.getWidth() * (1 - 0.24f), battleField.getY() + battleField.getHeight() * (1 - 0.0f));
        stage.addActor(nameTagSprite2);

        Label playerNameLabel = new Label(preferences.getString("userName"), UIConstants.labelStyleSmall);
        Float labelScale = 0.75f * nameTagSprite.getHeight()/playerNameLabel.getHeight();
        playerNameLabel.setColor(new Color(0x4169E1FF));
        playerNameLabel.setFontScale(labelScale);
        playerNameLabel.setBounds(nameTagSprite.getX(), nameTagSprite.getY(), nameTagSprite.getWidth(), nameTagSprite.getHeight());
        playerNameLabel.setAlignment(Align.center);
        stage.addActor(playerNameLabel);

        Label playerNameLabel2 = new Label(preferences.getString("opponentName"), UIConstants.labelStyleSmall);
        playerNameLabel2.setColor(new Color(0xFF3030FF));
        playerNameLabel2.setFontScale(labelScale);
        playerNameLabel2.setBounds(nameTagSprite2.getX(), nameTagSprite2.getY(), nameTagSprite2.getWidth(), nameTagSprite2.getHeight());
        playerNameLabel2.setAlignment(Align.center);
        stage.addActor(playerNameLabel2);

        Image turnSprite = new Image(nameTagTexture);
        turnSprite.setWidth(UIConstants.nameTagWidth * stage.getWidth());
        turnSprite.setHeight(UIConstants.nameTagHeight * stage.getWidth());
        turnSprite.setPosition(battleField.getX() + battleField.getWidth() * 0.3925f, battleField.getY() + battleField.getHeight() * (-0.165f));
        stage.addActor(turnSprite);

        turnLabel = new Label("", UIConstants.labelStyleSmall);
        turnLabel.setColor(new Color(0xFFFFFFFF));
        turnLabel.setFontScale(labelScale);
        turnLabel.setBounds(turnSprite.getX(), turnSprite.getY(), turnSprite.getWidth(), turnSprite.getHeight());
        turnLabel.setAlignment(Align.center);
        stage.addActor(turnLabel);

        updateTurnLabel();
    }

    void updateTurnLabel()
    {
        turnLabel.setText(battleField.battleFieldLogic.isLeftPlayerTurn ? "Blue Turn" : "Red Turn");
        turnLabel.setColor(new Color(battleField.battleFieldLogic.isLeftPlayerTurn ? 0x4169E1FF : 0xFF3030FF));
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

        continueButton = new ButtonNode(continueButtonTexture);
        continueButton.setWidth(UIConstants.menuButtonWidth * stage.getHeight());
        continueButton.setHeight(UIConstants.menuButtonHeight * stage.getHeight());

        quitButton = new ButtonNode(quitButtonTexture);
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
        continueButton.remove();
        quitButton.remove();

        backgroundTexture.dispose();
        pauseBackgroundTexture.dispose();
        pauseButtonTexture.dispose();
        turnButtonTexture.dispose();
        quitButtonTexture.dispose();
        continueButtonTexture.dispose();
        nameTagTexture.dispose();
    }

    Boolean paused = false;

    private void pauseAction() {
        if (!paused)
        {
            paused = true;
            battleField.setTouchable(Touchable.disabled);
            sliderGroup.setTouchable(Touchable.disabled);
            turnButton.setTouchable(Touchable.disabled);
            battleField.removePopUp();

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

    public void quitAction()
    {
        connectionHandler.quitGame();
        returnToMainMenu();
    }

    public void returnToMainMenu()
    {
        Preferences preferences = Gdx.app.getPreferences("UserData");
        preferences.putBoolean("gameRunning", false);
        preferences.flush();

        game.setScreen(new MainMenuScreen(game));
        dispose();
    }

    private void placeAction()
    {
        if (!battleField.battleFieldLogic.isLeftPlayerTurn || battleField.animationsRunning) return;
        updateMinionStats();
        sendFloatingMinion();
        battleField.placeFloatingMinion();
        updateTurnLabel();
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

    public void gameOver(Boolean leftWinner, Boolean quit)
    {
        battleField.setTouchable(Touchable.disabled);
        sliderGroup.setTouchable(Touchable.disabled);
        turnButton.setTouchable(Touchable.disabled);
        pauseButton.setTouchable(Touchable.disabled);


        GameOverGroup gameOverGroup = new GameOverGroup(this, leftWinner, quit);
        gameOverGroup.setPosition(0, 0);
        gameOverGroup.setWidth(stage.getWidth());
        gameOverGroup.setHeight(stage.getHeight());
        gameOverGroup.setup();
        stage.addActor(gameOverGroup);
    }



    void placeReceivedMinion(int x, int y, int[] values, Boolean leftPlayer, Boolean animated) {
        if (x == -1 || y == -1)
        {
            battleField.placeMinion(null, animated);
        }
        else
        {
            MinionNode minionNode = new MinionNode(leftPlayer);
            minionNode.minion.xPos = x;
            minionNode.minion.yPos = y;

            for (int i = 0; i < values.length; i++)
            {
                SliderType type = SliderType.values()[i];
                minionNode.minion.setAttribute(type.toString(), type.valueForStep(values[i]));
            }
            minionNode.minion.setAttribute("MaxHealth", minionNode.minion.getAttribute("Health"));
            minionNode.updateStats();

            battleField.placeMinion(minionNode, animated);
        }
        updateTurnLabel();
    }

    void sendFloatingMinion()
    {
        if (battleField.floatingMinion != null)
        {
            int x = battleField.floatingMinion.minion.xPos;
            if (!isFirstPlayer) x = 9-x;

            int y = battleField.floatingMinion.minion.yPos;
            int[] values = new int[8];
            for (int i = 0; i < 8; i++)
            {
                SliderNode slider = sliders.get(i);
                values[i] = slider.actualStep;
            }
            connectionHandler.sendMove(x, y, values);
        }
        else
        {
            connectionHandler.sendMove(-1, -1, new int[]{0,0,0,0,0,0,0,0});
        }
    }

    @Override
    public void gameFound(String token, int gameId, String opponentName, Boolean isFirstPlayer) {

    }

    @Override
    public void receivedMove(int x, int y, int[] values) {
        int xPos = x;
        if (x != -1 && y != -1 && !isFirstPlayer) xPos = 9-xPos;
        placeReceivedMinion(xPos, y, values, false, true);
    }

    @Override
    public void opponentQuit() {
        gameOver(true, true);
    }

    @Override
    public void restoredGame(int[] xs, int[] ys, int[][] valuesArray) {
        int turns = xs.length;

        for (int i = 0; i < turns; i++)
        {
            int x = xs[i];
            int y = ys[i];

            int xPos = x;
            if (x != -1 && y != -1 && !isFirstPlayer) xPos = 9-xPos;

            int[] values = valuesArray[i];
            placeReceivedMinion(xPos, y, values, battleField.battleFieldLogic.isLeftPlayerTurn, false);
        }

    }

    @Override
    public void confirmMove() {

    }

    @Override
    public void rejectMove() {

    }

    @Override
    public int historyStored() {
        return 0;
    }
}
