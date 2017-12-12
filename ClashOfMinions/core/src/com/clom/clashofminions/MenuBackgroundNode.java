package com.clom.clashofminions;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.Group;

/**
 * Created by greensn on 14.11.17.
 */

public class MenuBackgroundNode extends Actor {

    Texture backgroundTexture;

    public MenuBackgroundNode(){
        backgroundTexture = new Texture(Gdx.files.internal("Background-Menu.png"));
    }


    @Override
    public void draw(Batch batch, float parentAlpha) {

        batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());
    }

    @Override
    public boolean remove() {
        dispose();
        return super.remove();
    }

    @Override
    protected void setParent(Group parent) {
        if (parent == null) dispose();
        super.setParent(parent);
    }

    void dispose()
    {
        backgroundTexture.dispose();
    }
}
