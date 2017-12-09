package com.clom.clashofminions.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.math.GridPoint2;
import com.badlogic.gdx.math.Interpolation;
import com.badlogic.gdx.math.Vector2;
import com.badlogic.gdx.scenes.scene2d.Group;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;
import com.badlogic.gdx.scenes.scene2d.actions.Actions;
import com.badlogic.gdx.scenes.scene2d.ui.Image;
import com.badlogic.gdx.scenes.scene2d.utils.ActorGestureListener;
import com.badlogic.gdx.utils.Array;
import com.badlogic.gdx.utils.Timer;
import com.clom.clashofminions.GameScreen;
import com.clom.clashofminions.UIConstants;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Created by greensn on 09.11.17.
 */

public class BattleField extends Group {

    Texture backgroundTexture = new Texture(Gdx.files.internal("Battlefield.png"));
    Texture attackSymbol = new Texture(Gdx.files.internal("Symbol-Attack.png"));
    Texture healingSymbol = new Texture(Gdx.files.internal("Symbol-Healing.png"));

    public BattleFieldLogic battleFieldLogic;

    public MinionNode floatingMinion;
    public MinionNode storedMinion;
    public AtomicBoolean waitingForConfirm = new AtomicBoolean(false);
    public AtomicBoolean processingConfirm = new AtomicBoolean(false);

    public volatile int turnCount = 0;

    float tileWidth;
    float tileHeight;

    GameScreen game;

    public BattleField(GameScreen game)
    {
        this.game = game;
        Preferences preferences = Gdx.app.getPreferences("UserData");
        battleFieldLogic = new BattleFieldLogic(UIConstants.battleFieldTilesHorizontal,UIConstants.battleFieldTilesVertical);
    }

    public void setup()
    {
        tileHeight = getHeight()/UIConstants.battleFieldTilesVertical;
        tileWidth = getWidth()/UIConstants.battleFieldTilesHorizontal;

        Image bg = new Image(backgroundTexture);
        bg.setWidth(getWidth()/UIConstants.battleFieldTilesHorizontal * (UIConstants.battleFieldTilesHorizontal + 4));
        bg.setHeight(getHeight()/UIConstants.battleFieldTilesHorizontal * (UIConstants.battleFieldTilesHorizontal + 4));
        bg.setPosition(-(bg.getWidth() - getWidth())/2, -(bg.getHeight() - getHeight())/2);

        addActor(bg);

        addListener(new InputListener() {

            @Override
            public boolean touchDown(InputEvent event, float x, float y, int pointer, int button) {
                return draggedTo(x, y);
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

        addListener(new ActorGestureListener()
        {
            @Override
            public void tap(InputEvent event, float x, float y, int count, int button) {
                //System.out.println("Tapped " + x + " " + y);
                GridPoint2 coord = positionToCoordinates(new Vector2(x, y));
                MinionNode minionNode = battleFieldLogic.getMinionNode(coord.x, coord.y);

                if (minionNode != null)
                {
                    showPopUp(minionNode);
                }
            }
        });
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        super.draw(batch, parentAlpha);

        if (floatingMinion != null)
        {
            drawAttackPositions(batch);
            drawHealingPositions(batch);
        }
    }

    void drawAttackPositions(Batch batch)
    {
        int xMod = floatingMinion.minion.isLeftPlayer ? 1 : -1;
        Array<GridPoint2> attackPositions = new Array<GridPoint2>();
        switch(floatingMinion.minion.getAttribute("AttackRange")){
            case 3:
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 2 * xMod, floatingMinion.minion.yPos + 1));
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 2 * xMod, floatingMinion.minion.yPos - 1));
            case 2:
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 0 * xMod, floatingMinion.minion.yPos + 1));
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 0 * xMod, floatingMinion.minion.yPos - 1));
            case 1:
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 2 * xMod, floatingMinion.minion.yPos + 0));
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 1 * xMod, floatingMinion.minion.yPos + 1));
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 1 * xMod, floatingMinion.minion.yPos - 1));
            case 0:
                attackPositions.add(new GridPoint2(floatingMinion.minion.xPos + 1 * xMod, floatingMinion.minion.yPos + 0));
                break;
        }

        for (GridPoint2 position:attackPositions)
        {
            if (!isCoordInField(position)) continue;
            Vector2 pos = coordinatesToPosition(position);
            batch.draw(attackSymbol, getX() + pos.x, getY() + pos.y, tileWidth, tileHeight);
        }
    }

    void drawHealingPositions(Batch batch)
    {
        int xMod = floatingMinion.minion.isLeftPlayer ? 1 : -1;
        Array<GridPoint2> healingPositions = new Array<GridPoint2>();
        switch(floatingMinion.minion.getAttribute("BuffRange")){
            case 3:
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos - 1 * xMod, floatingMinion.minion.yPos + 1));
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos - 1 * xMod, floatingMinion.minion.yPos + 0));
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos - 1 * xMod, floatingMinion.minion.yPos - 1));
            case 2:
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos + 1 * xMod, floatingMinion.minion.yPos + 1));
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos + 1 * xMod, floatingMinion.minion.yPos + 0));
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos + 1 * xMod, floatingMinion.minion.yPos - 1));
            case 1:
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos + 0 * xMod, floatingMinion.minion.yPos + 1));
                healingPositions.add(new GridPoint2(floatingMinion.minion.xPos + 0 * xMod, floatingMinion.minion.yPos - 1));
            case 0:
                break;
        }

        for (GridPoint2 position:healingPositions)
        {
            if (!isCoordInField(position)) continue;
            Vector2 pos = coordinatesToPosition(position);
            batch.draw(healingSymbol, getX() + pos.x, getY() + pos.y, tileWidth, tileHeight);
        }
    }

    Boolean isCoordInField(GridPoint2 coord)
    {
        return !(coord.x < 0 ||
                coord.x > UIConstants.battleFieldTilesHorizontal-1 ||
                coord.y < 0 ||
                coord.y > UIConstants.battleFieldTilesVertical-1);
    }


    synchronized Boolean draggedTo(float x, float y)
    {
        if (animationsRunning) return false;
        if (!battleFieldLogic.isLeftPlayerTurn) return false;
        if(waitingForConfirm.get()) return false;
        GridPoint2 coord = positionToCoordinates(new Vector2(x, y));
        if (coord.x == -1 || coord.y == -1) return false;
        if (coord.x >= 4 && battleFieldLogic.isLeftPlayerTurn) return false;
        if (coord.x <= 5 && !battleFieldLogic.isLeftPlayerTurn) return false;

        if (battleFieldLogic.getMinionNode(coord.x, coord.y) != null) return false;

        if (floatingMinion == null) {
            floatingMinion = new MinionNode(battleFieldLogic.isLeftPlayerTurn); //only for left side player so far
            floatingMinion.isFloating = true;
            floatingMinion.setWidth(getWidth()/UIConstants.battleFieldTilesHorizontal);
            floatingMinion.setHeight(getHeight()/UIConstants.battleFieldTilesVertical);
            addActor(floatingMinion);
            game.updateMinionStats();
        }

        System.out.println("Dragged minion to: " + coord);

        Vector2 newPosition = coordinatesToPosition(coord);
        floatingMinion.setPosition(newPosition.x, newPosition.y);
        floatingMinion.minion.xPos = coord.x;
        floatingMinion.minion.yPos = coord.y;
        return true;
    }


    GridPoint2 positionToCoordinates(Vector2 pos)
    {
        float h = tileHeight;
        float w = tileWidth;

        int x = (int)Math.floor(pos.x/w);
        if (x < 0) x = -1;
        if (x > UIConstants.battleFieldTilesHorizontal-1) x = -1;
        int y = (int)Math.floor(pos.y/h);
        if (y < 0) y = -1;
        if (y > UIConstants.battleFieldTilesVertical-1) y = -1;

        return new GridPoint2(x, y);
    }

    Vector2 coordinatesToPosition(GridPoint2 coord)
    {
        float h = getHeight()/UIConstants.battleFieldTilesVertical;
        float w = getWidth()/UIConstants.battleFieldTilesHorizontal;

        return new Vector2(w * coord.x, h * coord.y);
    }

    public synchronized void queuePlacement(){
        if(waitingForConfirm.compareAndSet(false,true)) {
            storedMinion = floatingMinion;
            if(storedMinion == null){
                waitingForConfirm.set(false);
                System.out.println("No minion assigned");
            }
        }
    }

    public void confirmPlacement(){
        placeFloatingMinion();
    }

    public void rejectPlacement(){
      if(processingConfirm.compareAndSet(false, true)){
        if(waitingForConfirm.get()) {
          storedMinion = null;
          waitingForConfirm.set(false);
        }
        processingConfirm.set(false);
      }

    }

    public void placeFloatingMinion()
    {
      if(processingConfirm.compareAndSet(false, true)) {
        if (waitingForConfirm.get()) {
          placeMinion(storedMinion, true);
          storedMinion = null;
          waitingForConfirm.set(false);
        }
        processingConfirm.set(false);
      }
    }

    public Boolean animationsRunning = false;
    MinionNode minionWaitingForPlacement;

    public void placeMinion(MinionNode minionNode, Boolean animated)
    {
        ++turnCount;
        if (battleFieldLogic.gameOver) return;
        if (animationsRunning)
        {
            minionWaitingForPlacement = minionNode;
            return;
        }
        Boolean gameOver;
        if (minionNode != null)
        {
            Boolean occupied = battleFieldLogic.field[minionNode.minion.xPos][minionNode.minion.yPos] != null;
            if (occupied)
            {
                placeMinion(null, animated);
                return;
            }

            if (!minionNode.hasParent())
            {
                Vector2 pos = coordinatesToPosition(new GridPoint2(minionNode.minion.xPos, minionNode.minion.yPos));
                minionNode.setPosition(pos.x, pos.y);
                minionNode.setWidth(getWidth()/UIConstants.battleFieldTilesHorizontal);
                minionNode.setHeight(getHeight()/UIConstants.battleFieldTilesVertical);
                addActor(minionNode);
            }

            gameOver = battleFieldLogic.addMinionAsTurn(minionNode, minionNode.minion.xPos, minionNode.minion.yPos, minionNode.minion.isLeftPlayer);
            minionNode.updateStats();
            minionNode.isFloating = false;
        }
        else
        {
            gameOver = battleFieldLogic.doGameStep();
        }

        if (gameOver)
        {
            game.gameOver(battleFieldLogic.isLeftPlayerTurn, false);
        }

        float moveDuration = runMoveAnimations(0, animated);
        float healDuration = runHealAnimations(moveDuration, animated);
        float attackDuration = runAttackAnimations(moveDuration + healDuration, animated);

        if (animated)
        {
            animationsRunning = true;
            Timer.schedule(new Timer.Task(){
                @Override
                public void run() {
                    animationsRunning = false;
                    if (minionWaitingForPlacement != null) placeMinion(minionWaitingForPlacement, true);
                    minionWaitingForPlacement = null;
                }
            }, moveDuration + healDuration + attackDuration);
        }
    }

    float runMoveAnimations(float delay, boolean animated)
    {
        removePopUp();

        float duration = 0.5f;
        for (MinionNode node : battleFieldLogic.movedMinions)
        {
            Vector2 newPosition = coordinatesToPosition(new GridPoint2(node.minion.xPos, node.minion.yPos));
            if (animated) node.addAction(Actions.moveTo(newPosition.x, newPosition.y, duration, Interpolation.smooth));
            else node.setPosition(newPosition.x, newPosition.y);
            //node.setPosition(newPosition.x, newPosition.y);
        }
        return duration;
    }

    float runHealAnimations(float delay, Boolean animated)
    {
        float duration = 0.5f;
        float sDelay = 0f;
        for (BattleFieldLogic.Event event : battleFieldLogic.minionHeals)
        {
            final MinionNode n1 = event.src;
            for (int i = 0; i < event.targets.size(); i++) {
                final MinionNode n2 = event.targets.get(i);
                if (animated) healAnimation(
                        new GridPoint2(n1.minion.xPos, n1.minion.yPos),
                        new GridPoint2(n2.minion.xPos, n2.minion.yPos),
                        n2,
                        event.value1.get(i),
                        delay + sDelay,
                        duration);
                else
                {
                    n2.updateStats();
                }
            }
            sDelay += duration;
        }
        return sDelay;
    }

    float runAttackAnimations(float delay, Boolean animated)
    {
        float duration = 0.5f;
        float sDelay = 0f;
        for (BattleFieldLogic.Event event : battleFieldLogic.minionAttacks)
        {
            final MinionNode n1 = event.src;
            for (int i = 0; i < event.targets.size(); i++) {
                final MinionNode n2 = event.targets.get(i);
                final boolean lethal = event.lethal.get(i);
                if (animated) shootProjectile(
                        new GridPoint2(n1.minion.xPos, n1.minion.yPos),
                        new GridPoint2(n2.minion.xPos, n2.minion.yPos),
                        n2,
                        event.value1.get(i),
                        delay + sDelay,
                        duration,
                        event.value2.get(i));
                else
                {
                    n2.updateStats();
                }

                if (lethal) {
                    if (animated) n2.addAction(Actions.sequence(
                            Actions.delay(delay + sDelay + duration),
                            Actions.fadeOut(0.2f),
                            Actions.removeActor()));
                    else
                    {
                        n2.remove();
                    }
                }
            }
            sDelay += duration;
        }
        return sDelay;
    }

    void shootProjectile(GridPoint2 from, GridPoint2 to, final MinionNode targetMinion, final int health, float delay, float duration, final int shield)
    {
        Vector2 p1 = coordinatesToPosition(from);
        Vector2 p2 = coordinatesToPosition(to);

        Projectile projectile = new Projectile("Projectile.png");
        projectile.setPosition(p1.x, p1.y);
        projectile.setWidth(tileWidth);
        projectile.setHeight(tileHeight);
        addActor(projectile);
        projectile.setVisible(false);
        projectile.setScale(0, 0);
        projectile.addAction(Actions.sequence(
                Actions.delay(delay),
                Actions.show(),
                Actions.parallel(
                        Actions.moveTo(p2.x, p2.y, duration * 1.0f, Interpolation.smooth),
                        Actions.sequence(
                                Actions.scaleTo(2, 2, duration * 0.5f, Interpolation.pow2Out),
                                Actions.scaleTo(0, 0, duration * 0.5f, Interpolation.pow2In))
                        ),
                Actions.run(new Runnable() {
                    @Override
                    public void run() {
                        targetMinion.setHealth(health);
                        targetMinion.setShield(shield);
                    }
                }),
                Actions.removeActor()));
    }

    void healAnimation(GridPoint2 from, GridPoint2 to, final MinionNode targetMinion, final int health, float delay, float duration)
    {
        Vector2 p1 = coordinatesToPosition(from);
        Vector2 p2 = coordinatesToPosition(to);

        Projectile projectile = new Projectile("Heart.png");
        projectile.setPosition(p1.x, p1.y);
        projectile.setWidth(tileWidth);
        projectile.setHeight(tileHeight);
        addActor(projectile);
        projectile.setVisible(false);
        projectile.addAction(Actions.sequence(
                Actions.delay(delay),
                Actions.show(),
                Actions.moveTo(p2.x, p2.y, duration * 1.0f, Interpolation.smooth),
                Actions.run(new Runnable() {
                    @Override
                    public void run() {
                        targetMinion.setHealth(health);
                    }
                }),
                Actions.parallel(
                        Actions.scaleTo(3f, 3f, 1f, Interpolation.pow2In),
                        Actions.fadeOut(1f, Interpolation.pow2In)
                        ),
                Actions.removeActor()));
    }

    DetailPopUpNode visiblePopUp;

    void showPopUp(MinionNode minionNode)
    {
        if (visiblePopUp != null && visiblePopUp.hasParent() && visiblePopUp.minionNode == minionNode)
        {
            removePopUp();
            return;
        }
        removePopUp();

        Float x = getX() + minionNode.getX() + minionNode.getWidth();
        Float y = getY() + minionNode.getY() - minionNode.getHeight();
        visiblePopUp = new DetailPopUpNode(minionNode, tileWidth * 2, tileHeight * 3);
        visiblePopUp.setPosition(x, y);
        getStage().addActor(visiblePopUp);
    }

    public void removePopUp()
    {
        if (visiblePopUp != null)
        {
            visiblePopUp.remove();
            visiblePopUp = null;
        }
    }

    public void reset(){
        //TODO: confirm this resets the battlefield without memory leaks or other issues
        this.clearChildren();
        this.clearActions();
        battleFieldLogic.reset();
    }
}
