package com.clom.clashofminions.Nodes;

import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;

/**
 * Created by greensn on 07.11.17.
 */

public class ButtonNode extends Actor {

    Texture backgroundTexture;


    public ButtonNode(Texture texture)
    {
        backgroundTexture = texture;

        addListener(new InputListener() {

            @Override
            public boolean touchDown(InputEvent event, float x, float y, int pointer, int button) {
                setScale(0.8f);
                return true;
            }

            @Override
            public void touchDragged(InputEvent event, float x, float y, int pointer) {
                movedTo(x, y);

                super.touchDragged(event, x, y, pointer);
            }

            @Override
            public void enter(InputEvent event, float x, float y, int pointer, Actor fromActor) {
                setScale(1.1f);

                super.enter(event, x, y, pointer, fromActor);
            }

            @Override
            public void exit(InputEvent event, float x, float y, int pointer, Actor toActor) {
                setScale(1f);

                super.exit(event, x, y, pointer, toActor);
            }

            @Override
            public void touchUp(InputEvent event, float x, float y, int pointer, int button) {
                setScale(1f);
            }

        });

    }

    void movedTo(float x, float y)
    {
        if (x < 0 || y < 0 || x > getWidth() || y > getHeight())
        {
            setScale(1f);
        }
        else
        {
            setScale(0.8f);
        }
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        float w = getWidth() * getScaleX();
        float h = getHeight() * getScaleY();
        batch.draw(backgroundTexture, getX() - (w - getWidth())/2, getY() - (h - getHeight())/2, w, h);
    }
}
