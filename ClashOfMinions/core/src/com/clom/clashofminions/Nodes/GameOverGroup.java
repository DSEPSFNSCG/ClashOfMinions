package com.clom.clashofminions.Nodes;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;
import com.badlogic.gdx.math.GridPoint2;
import com.badlogic.gdx.math.Vector2;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.badlogic.gdx.scenes.scene2d.ui.Label;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.utils.ActorGestureListener;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.clom.clashofminions.BattleField.MinionNode;
import com.clom.clashofminions.GameScreen;
import com.clom.clashofminions.UIConstants;

/**
 * Created by greensn on 04.12.17.
 */

public class GameOverGroup extends Group {

    Boolean leftPlayerWon;
    Boolean quit;

    GameScreen game;


    public GameOverGroup (GameScreen game, Boolean leftPlayerWon, Boolean quit)
    {
        this.game = game;
        this.leftPlayerWon = leftPlayerWon;
        this.quit = quit;
    }

    public void setup()
    {
        Texture pauseBackgroundTexture = new Texture(Gdx.files.internal("PauseMenu-Background.png"));
        Image bgSprite = new Image(pauseBackgroundTexture);
        bgSprite.setWidth(getWidth());
        bgSprite.setHeight(getHeight());

        Image victoryText = new Image(new Texture(Gdx.files.internal(leftPlayerWon ? "BlueVictoryText.png" : "RedVictoryText.png")));
        victoryText.setWidth(getWidth()*0.4f);
        victoryText.setHeight(victoryText.getWidth()*0.24375f);
        victoryText.setPosition(getWidth()/2 - victoryText.getWidth()/2, getHeight()/2 );

        Label label = new Label((quit ? "Opponent left the game!" : ""), game.game.skin);
        label.setColor(Color.WHITE);
        final GlyphLayout nameLayout2 = new GlyphLayout(UIConstants.font, label.getText());
        label.setPosition(getWidth()/2 - nameLayout2.width/2, getHeight() * 0.8f);


        addActor(bgSprite);
        addActor(victoryText);
        addActor(label);

        addListener(new ActorGestureListener()
        {
            @Override
            public void tap(InputEvent event, float x, float y, int count, int button) {
                game.returnToMainMenu();
            }
        });
    }
}
