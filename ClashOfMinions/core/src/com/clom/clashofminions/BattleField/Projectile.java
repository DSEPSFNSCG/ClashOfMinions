package com.clom.clashofminions.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;

/**
 * Created by greensn on 15.11.17.
 */

public class Projectile extends Actor{

    Texture texture;

    Projectile(String name)
    {
        texture = new Texture(Gdx.files.internal(name));
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        Color color = getColor();
        batch.setColor(color.r, color.g, color.b, color.a * parentAlpha);

        float w = getWidth() * getScaleX();
        float h = getHeight() * getScaleY();
        batch.draw(texture, getX() - (w - getWidth())/2, getY() - (h - getHeight())/2, w, h);

        batch.setColor(color.r, color.g, color.b, 1f);
    }
}
