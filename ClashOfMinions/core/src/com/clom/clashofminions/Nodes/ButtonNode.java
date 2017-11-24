package com.clom.clashofminions.Nodes;

import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;

/**
 * Created by greensn on 07.11.17.
 */

public class ButtonNode extends Actor {

    Texture backgroundTexture;


    public ButtonNode(Texture texture)
    {
        backgroundTexture = texture;
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());
    }
}
