package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.math.GridPoint2;
import com.badlogic.gdx.math.Vector2;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;
import com.badlogic.gdx.scenes.scene2d.utils.ActorGestureListener;
import com.badlogic.gdx.utils.Array;
import com.mygdx.game.Nodes.SliderType;
import com.mygdx.game.UIConstants;

/**
 * Created by greensn on 20.11.17.
 */

public class DetailPopUpNode extends Group {

    MinionNode minionNode;

    Texture backgroundTexture;
    Array<Texture> icons = new Array<Texture>();
    Array<String> values = new Array<String>();

    DetailPopUpNode(MinionNode minionNode, Float width, Float height)
    {
        this.minionNode = minionNode;

        setWidth(width);
        setHeight(height);

        setupTextures();
        loadValues();

        addListener(new ActorGestureListener()
        {
            @Override
            public void tap(InputEvent event, float x, float y, int count, int button) {
                System.out.println("Close PopUp");
                remove();
            }
        });
    }

    void setupTextures()
    {
        backgroundTexture =  new Texture(Gdx.files.internal("PopUpBackground.png"));

        icons.add(new Texture(Gdx.files.internal("Icon-Heart.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-Heart.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-Sword-Crossed.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-Healing.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-AttackRange.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-HealingRange.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-Heart.png")));
        icons.add(new Texture(Gdx.files.internal("Icon-Heart.png")));
    }

    void loadValues()
    {
        values.add(minionNode.minion.getAttribute(SliderType.Health.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.Shield.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.Attack.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.HealingPower.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.AttackRange.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.HealBuffRange.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.AttackBoost.toString()) + "");
        values.add(minionNode.minion.getAttribute(SliderType.HealBuffBoost.toString()) + "");
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {

        batch.draw(backgroundTexture, getX(), getY(), getWidth(), getHeight());

        Float borderGapY = getWidth()/2 * 0.3f;
        Float borderGapX = getWidth()/2 * 0.3f;
        Float textGapX = getWidth()/2 * 0.025f;

        Float rowHeight = (getHeight()-(5*borderGapY))/4;
        Float columnWidth = getWidth()/2;

        Float textPositionX = borderGapX + rowHeight + textGapX;
        Float textPositionY = rowHeight * 0.4f;

        Float xLeft = getX();
        Float yTop = getY() + getHeight();

        for(int i = 0; i < 2; i++)
        {
            for(int k = 0; k < 4; k++)
            {
                Texture iconTexture = icons.get(i*4+k);
                String text = values.get(i*4+k);

                Float yPos = yTop - (k+1)*(borderGapY + rowHeight);
                batch.draw(iconTexture, xLeft + i * columnWidth + borderGapX, yPos, rowHeight, rowHeight);
                UIConstants.font.draw(batch, text, xLeft + i * columnWidth + textPositionX, yPos + textPositionY);
            }
        }
    }
}
