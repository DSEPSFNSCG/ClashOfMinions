package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.mygdx.game.UIConstants;

/**
 * Created by greensn on 09.11.17.
 */

public class MinionNode extends Actor {
    public final Minion minion;

    Texture minionTexture;
    int health;

    public MinionNode(boolean isLeftPlayer){
        minion = new Minion(isLeftPlayer);

        if (minion.isLeftPlayer) {minionTexture = new Texture(Gdx.files.internal("Minion-Blue.png"));}
        else {
            minionTexture = new Texture(Gdx.files.internal("Minion-Red.png"));
        }
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        Color color = getColor();
        batch.setColor(color.r, color.g, color.b, color.a * parentAlpha);

        batch.draw(minionTexture, getX(), getY(), getWidth(), getHeight());
        UIConstants.font.draw(batch, "" + health, getX(), getY());

        batch.setColor(color.r, color.g, color.b, 1f);
    }

    public void updateHealth()
    {
        health = minion.getAttribute("Health");
    }
}
