package com.mygdx.game.Nodes;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.graphics.g3d.Material;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;

/**
 * Created by greensn on 08.11.17.
 */

public class SliderNode extends Actor {

    Texture backgroundTexture;
    Texture sliderFillTexture;
    Texture sliderHandle;

    public SliderType type;
    int sliderSteps;
    public int actualStep = 0;

    ManaBarNode manaBarNode;

    public SliderNode(SliderType type, ManaBarNode manaBar)
    {
        manaBarNode = manaBar;
        this.type = type;
        sliderSteps = type.steps();

        backgroundTexture = new Texture(Gdx.files.internal("Button-Menu.png"));
        sliderFillTexture = new Texture(Gdx.files.internal("Button-Menu.png"));
        sliderHandle = new Texture(Gdx.files.internal("Button-Menu.png"));

        addListener(new InputListener() {

            @Override
            public boolean touchDown(InputEvent event, float x, float y, int pointer, int button) {
                System.out.println("x: " + x + ", y: " + y);
                draggedTo(x, y);
                return true;
            }

            @Override
            public void touchDragged(InputEvent event, float x, float y, int pointer) {
                draggedTo(x, y);

                super.touchDragged(event, x, y, pointer);
            }

            @Override
            public void touchUp(InputEvent event, float x, float y, int pointer, int button) {
                draggedTo(x, y);
            }

        });
    }

     @Override
     public void draw(Batch batch, float parentAlpha) {
         float h = getHeight()/(sliderSteps-1);
         float sliderHeight = 0.1f*getHeight();

         batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());
         batch.draw(sliderFillTexture, getX(), getY(), getWidth(), h * actualStep);
         batch.draw(sliderHandle, getX(), getY() - sliderHeight/2 + h * actualStep, getWidth(), sliderHeight);
     }

     void draggedTo(float x, float y)
     {
         //float relPos = y/getHeight();
         float h = getHeight()/(sliderSteps-1);
         float step = (y + 0.5f*h)/h;
         setSteps((int)Math.floor((double)step));

     }

     void setSteps(int steps)
     {
         manaBarNode.actualStep += actualStep;

         int maxPossibleSteps = manaBarNode.actualStep;
         if (maxPossibleSteps > sliderSteps-1) maxPossibleSteps = sliderSteps-1;

         actualStep = steps;
         if (actualStep > maxPossibleSteps) actualStep = maxPossibleSteps;
         if (actualStep < 0) actualStep = 0;

         manaBarNode.actualStep -= actualStep;
     }

     public int getSliderValue()
     {
         return type.valueForStep(actualStep);
     }
}
