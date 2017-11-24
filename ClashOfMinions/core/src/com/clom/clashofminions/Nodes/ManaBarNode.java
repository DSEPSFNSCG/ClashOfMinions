package com.clom.clashofminions.Nodes;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;

/**
 * Created by greensn on 09.11.17.
 */

public class ManaBarNode extends Actor {

    Texture backgroundTexture;
    Texture sliderFillTexture;

    public int sliderSteps;
    public int actualStep = 0;

    public ManaBarNode(int steps)
    {
        sliderSteps = steps;

        backgroundTexture = new Texture(Gdx.files.internal("Slider.png"));
        sliderFillTexture = new Texture(Gdx.files.internal("Slider-Fill.png"));
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        float h = getHeight()/(sliderSteps);
        float sliderHeight = 0.1f*getHeight();

        batch.draw(sliderFillTexture, getX(), getY(), getWidth(), h * actualStep);
        batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());
    }
}
