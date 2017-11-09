package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.math.GridPoint2;
import com.badlogic.gdx.math.Vector;
import com.badlogic.gdx.math.Vector2;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.mygdx.game.GameScreen;
import com.mygdx.game.UIConstants;

/**
 * Created by greensn on 09.11.17.
 */

public class BattleField extends Group {

    Texture backgroundTexture = new Texture(Gdx.files.internal("BattlefieldBackground.png"));

    MinionNode floatingMinion;
    GameScreen game;

    public BattleField(GameScreen game)
    {
        this.game = game;
    }

    public void setup()
    {
        Image bg = new Image(backgroundTexture);
        bg.setWidth(getWidth());
        bg.setHeight(getHeight());

        addActor(bg);

        addListener(new InputListener() {

            @Override
            public boolean touchDown(InputEvent event, float x, float y, int pointer, int button) {
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

    void draggedTo(float x, float y)
    {
        GridPoint2 coord = positionToCoordinates(new Vector2(x, y));
        if (coord.x >= 4) return;

        if (floatingMinion == null) {
            floatingMinion = new MinionNode();
            floatingMinion.setWidth(getWidth()/UIConstants.battleFieldTilesHorizontal);
            floatingMinion.setHeight(getHeight()/UIConstants.battleFieldTilesVertical);
            addActor(floatingMinion);
        }

        System.out.println("Dragged minion to: " + coord);

        Vector2 newPosition = coordinatesToPosition(coord);
        floatingMinion.setPosition(newPosition.x, newPosition.y);
    }

    GridPoint2 positionToCoordinates(Vector2 pos)
    {
        float h = getHeight()/UIConstants.battleFieldTilesVertical;
        float w = getWidth()/UIConstants.battleFieldTilesHorizontal;

        int x = (int)Math.floor(pos.x/w);
        if (x < 0) x = 0;
        if (x > UIConstants.battleFieldTilesHorizontal-1) x = UIConstants.battleFieldTilesHorizontal-1;
        int y = (int)Math.floor(pos.y/h);
        if (y < 0) y = 0;
        if (y > UIConstants.battleFieldTilesVertical-1) y = UIConstants.battleFieldTilesVertical-1;

        return new GridPoint2(x, y);
    }

    Vector2 coordinatesToPosition(GridPoint2 coord)
    {
        float h = getHeight()/UIConstants.battleFieldTilesVertical;
        float w = getWidth()/UIConstants.battleFieldTilesHorizontal;

        return new Vector2(w * coord.x, h * coord.y);
    }
}
