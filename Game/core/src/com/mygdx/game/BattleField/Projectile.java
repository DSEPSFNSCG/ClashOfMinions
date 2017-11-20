package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
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
        batch.draw(texture, getX(), getY(), getWidth(), getHeight());
    }
}
