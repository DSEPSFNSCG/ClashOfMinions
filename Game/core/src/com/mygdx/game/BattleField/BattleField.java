package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.math.GridPoint2;
import com.badlogic.gdx.math.Interpolation;
import com.badlogic.gdx.math.Vector2;
import com.badlogic.gdx.scenes.scene2d.Action;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;
import com.badlogic.gdx.scenes.scene2d.actions.Actions;
import com.badlogic.gdx.scenes.scene2d.actions.MoveToAction;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.mygdx.game.GameScreen;
import com.mygdx.game.UIConstants;

/**
 * Created by greensn on 09.11.17.
 */

public class BattleField extends Group {

    Texture backgroundTexture = new Texture(Gdx.files.internal("BattlefieldBackground.png"));
    public BattleFieldLogic battleFieldLogic;

    public MinionNode floatingMinion;
    GameScreen game;

    public BattleField(GameScreen game)
    {
        this.game = game;
        battleFieldLogic = new BattleFieldLogic(UIConstants.battleFieldTilesHorizontal,UIConstants.battleFieldTilesVertical);
    }

    public void setup()
    {
        Image bg = new Image(backgroundTexture);
        bg.setWidth(getWidth()/UIConstants.battleFieldTilesHorizontal * (UIConstants.battleFieldTilesHorizontal + 2));
        bg.setHeight(getHeight()/UIConstants.battleFieldTilesHorizontal * (UIConstants.battleFieldTilesHorizontal + 2));
        bg.setPosition(-(bg.getWidth() - getWidth())/2, -(bg.getHeight() - getHeight())/2);

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
        if (coord.x >= 4 && battleFieldLogic.isLeftPlayerTurn) return;
        if (coord.x <= 5 && !battleFieldLogic.isLeftPlayerTurn) return;

        if (battleFieldLogic.getMinionNode(coord.x, coord.y) != null) return;

        if (floatingMinion == null) {
            floatingMinion = new MinionNode(battleFieldLogic.isLeftPlayerTurn); //only for left side player so far
            floatingMinion.setWidth(getWidth()/UIConstants.battleFieldTilesHorizontal);
            floatingMinion.setHeight(getHeight()/UIConstants.battleFieldTilesVertical);
            addActor(floatingMinion);
        }

        //System.out.println("Dragged minion to: " + coord);

        Vector2 newPosition = coordinatesToPosition(coord);
        floatingMinion.setPosition(newPosition.x, newPosition.y);
        floatingMinion.minion.xPos = coord.x;
        floatingMinion.minion.yPos = coord.y;
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

    public void placeFloatingMinion()
    {
        Boolean gameOver = false;

        if (floatingMinion != null)
        {
            gameOver = battleFieldLogic.addMinionAsTurn(floatingMinion, floatingMinion.minion.xPos, floatingMinion.minion.yPos, floatingMinion.minion.isLeftPlayer);
            floatingMinion.updateHealth();
            floatingMinion = null;
        }
        else
        {
            gameOver = battleFieldLogic.doGameStep();
        }

        if (gameOver)
        {
            game.gameOver();
        }

        float moveDuration = runMoveAnimations(0);
        float attackDuration = runAttackAnimations(moveDuration);
    }

    float runMoveAnimations(float delay)
    {
        float duration = 0.5f;
        for (MinionNode node : battleFieldLogic.movedMinions)
        {
            Vector2 newPosition = coordinatesToPosition(new GridPoint2(node.minion.xPos, node.minion.yPos));
            node.addAction(Actions.moveTo(newPosition.x, newPosition.y, duration, Interpolation.smooth));
            //node.setPosition(newPosition.x, newPosition.y);
        }
        return duration;
    }

    float runAttackAnimations(float delay)
    {
        float duration = 0.5f;
        float sDelay = 0f;
        for (BattleFieldLogic.Event event : battleFieldLogic.minionAttacks)
        {
            final MinionNode n1 = event.src;
            for (int i = 0; i < event.targets.size(); i++) {
                final MinionNode n2 = event.targets.get(i);
                final int newHealth = n2.health-event.value;
                final boolean lethal = event.lethal.get(i);
                Vector2 p1 = coordinatesToPosition(new GridPoint2(n1.minion.xPos, n1.minion.yPos));
                Vector2 p2 = coordinatesToPosition(new GridPoint2(n2.minion.xPos, n2.minion.yPos));
                n1.addAction(Actions.sequence(
                        Actions.delay(delay + sDelay),
                        Actions.moveTo(p2.x, p2.y, duration * 0.4f, Interpolation.pow2In),
                        Actions.run(new Runnable() {
                            @Override
                            public void run() {
                                n2.setHealth(newHealth);
                            }
                        }),
                        Actions.moveTo(p1.x, p1.y, duration * 0.6f, Interpolation.elasticOut)));
                if (lethal) {
                    n2.addAction(Actions.sequence(
                            Actions.delay(0.95f),
                            Actions.fadeOut(0.2f),
                            Actions.removeActor()));
                }
                sDelay += duration;
            }
        }
        return duration;
    }
}
