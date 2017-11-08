package com.mygdx.game.Nodes;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;

/**
 * Created by greensn on 07.11.17.
 */

public class ButtonNode extends Actor {

    Texture texture = new Texture(Gdx.files.internal("badlogic.jpg"));

    @Override
    public void draw(Batch batch, float parentAlpha) {
        batch.draw(texture,0,0);
    }
}
