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

    Texture minionTexture = new Texture(Gdx.files.internal("Minion.png"));

    @Override
    public void draw(Batch batch, float parentAlpha) {
        batch.draw(minionTexture, getX(), getY(), getWidth(), getHeight());
    }

    public MinionNode(boolean isLeftPlayer){
        super();
        minion = new Minion(isLeftPlayer);
    }
}
