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
    int shield;

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
        UIConstants.font.setColor(Color.WHITE);
        UIConstants.font.draw(batch, "" + health, getX(), getY());
        UIConstants.font.setColor(Color.YELLOW);
        UIConstants.font.draw(batch, "" + shield, getX(), getY() + 12 );
        //TODO: Draw shield

        batch.setColor(color.r, color.g, color.b, 1f);
    }

    public void updateHealth()
    {
        this.health = minion.getAttribute("Health");
        this.shield = minion.getAttribute("Shield");
    }

    public void setShield(int shield) {
        this.shield = shield;
    }
    public void setHealth(int health)
    {
        this.health = health;
    }

    @Override
    public String toString(){
        return "MinionNode at " +  minion.yPos + ", " + minion.xPos;
    }
}
