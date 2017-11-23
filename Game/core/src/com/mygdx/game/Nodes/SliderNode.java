package com.mygdx.game.Nodes;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.graphics.g2d.TextureRegion;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;
import com.badlogic.gdx.scenes.scene2d.utils.TextureRegionDrawable;
import com.mygdx.game.GameScreen;
import com.mygdx.game.UIConstants;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;

/**
 * Created by greensn on 08.11.17.
 */

public class SliderNode extends Actor {

    Texture backgroundTexture;
    Texture sliderFillTexture;
    Texture sliderHandle;
    Texture iconTexture;

    public SliderType type;
    int sliderSteps;
    public int actualStep = 0;

    ManaBarNode manaBarNode;
    public GameScreen gameScreen;

    public SliderNode(SliderType type, ManaBarNode manaBar)
    {
        manaBarNode = manaBar;
        this.type = type;
        sliderSteps = type.steps();

        backgroundTexture = new Texture(Gdx.files.internal("Slider.png"));
        sliderFillTexture = new Texture(Gdx.files.internal("Slider-Fill.png"));
        sliderHandle = new Texture(Gdx.files.internal("Button-Menu.png"));
        iconTexture = new Texture(Gdx.files.internal(type.iconName()));
    }

    public void setup()
    {
//        regions = new TextureRegion[sliderSteps];
//        for (int i = 0; i < sliderSteps; i++)
//        {
//            TextureRegion r = new TextureRegion(sliderFillTexture, 0, 0, backgroundTexture.getWidth(), backgroundTexture.getHeight()/(sliderSteps-1) * i);
//            regions[i] = r;
//        }

        TextureRegionDrawable drawable = new TextureRegionDrawable(new TextureRegion(sliderFillTexture));

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
         float h = (getHeight() * 0.98f)/(sliderSteps-1);
         float sliderHeight = 0.1f*getHeight();

         float iconSize = getWidth() * 0.5f;

         batch.draw(sliderFillTexture, getX(), getY(), getWidth(), h * actualStep);
         batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());
         batch.draw(iconTexture, getX() - getWidth() * 0.05f - iconSize, getY() + iconSize*0.1f, iconSize, iconSize);
         //batch.draw(sliderHandle, getX(), getY() - sliderHeight/2 + h * actualStep, getWidth(), sliderHeight);

         final GlyphLayout layout = new GlyphLayout(UIConstants.font, "" + type.valueForStep(actualStep));

         float yOffset = actualStep == 0 ? getHeight() * 0.2f : 0f;

         UIConstants.font.draw(batch, "" + type.valueForStep(actualStep), getX() + (getWidth() - layout.width)/2, getY() - sliderHeight/2 + yOffset + h * actualStep);
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
         int oldActualStep = actualStep;

         manaBarNode.actualStep += actualStep;

         int maxPossibleSteps = manaBarNode.actualStep;
         if (maxPossibleSteps > sliderSteps-1) maxPossibleSteps = sliderSteps-1;

         actualStep = steps;
         if (actualStep > maxPossibleSteps) actualStep = maxPossibleSteps;
         if (actualStep < 0) actualStep = 0;

         manaBarNode.actualStep -= actualStep;

         if (oldActualStep != actualStep && gameScreen != null)
         {
             gameScreen.updateMinionStats();
         }
     }

     public int getSliderValue()
     {
         return type.valueForStep(actualStep);
     }
}
