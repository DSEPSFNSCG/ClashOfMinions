package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;

/**
 * Created by greensn on 14.11.17.
 */

public class GameBackgroundNode extends Actor {
    Texture backgroundTexture;

    public GameBackgroundNode(){
        backgroundTexture = new Texture(Gdx.files.internal("Background-Game.png"));
    }


    @Override
    public void draw(Batch batch, float parentAlpha) {

        batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());
    }
}
