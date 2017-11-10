package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
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

    public MinionNode(boolean isLeftPlayer){
        minion = new Minion(isLeftPlayer);

        if (minion.isLeftPlayer) {minionTexture = new Texture(Gdx.files.internal("Minion-Blue.png"));}
        else {minionTexture = new Texture(Gdx.files.internal("Minion-Red.png"));}
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        batch.draw(minionTexture, getX(), getY(), getWidth(), getHeight());
    }
}
