package com.mygdx.game.BattleField;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.mygdx.game.Nodes.SliderType;
import com.mygdx.game.UIConstants;

/**
 * Created by greensn on 09.11.17.
 */

public class MinionNode extends Actor {
    public final Minion minion;

    Texture minionTexture;
    Texture healthBar;
    Texture healthFill;
    Texture shieldFill;
    Texture sword;
    Texture swordFlipped;

    int health;
    int shield;

    public Boolean isFloating = false;

    public MinionNode(boolean isLeftPlayer){
        minion = new Minion(isLeftPlayer);

        setupTextures();
    }

    void setupTextures()
    {
        if (minion.isLeftPlayer) {minionTexture = new Texture(Gdx.files.internal("Minion-Blue.png"));}
        else {
            minionTexture = new Texture(Gdx.files.internal("Minion-Red.png"));
        }

        healthBar = new Texture(Gdx.files.internal("Slider-Health.png"));
        healthFill = new Texture(Gdx.files.internal("Slider-Fill-Health.png"));
        shieldFill = new Texture(Gdx.files.internal("Button-Menu.png"));
        sword = new Texture(Gdx.files.internal("Icon-Sword.png"));
        swordFlipped = new Texture(Gdx.files.internal("Icon-Sword-Flipped.png"));
    }

    @Override
    public void draw(Batch batch, float parentAlpha) {
        Color color = getColor();
        batch.setColor(color.r, color.g, color.b, color.a * parentAlpha);

        batch.draw(minionTexture, getX(), getY(), getWidth(), getHeight());
        UIConstants.font.draw(batch, "" + health, getX(), getY());
        Float healthBarPositionX = getX() + getWidth() * 0.05f;
        Float healthBarPositionY = getY() + getHeight() * 0.1f;
        Float healthBarWidth = getWidth() * 0.15f;
        Float healthBarHeight = getHeight() * 0.8f;

        if (!minion.isLeftPlayer)
        {
            healthBarPositionX = getX() + getWidth() - getWidth() * 0.05f - healthBarWidth;
        }

        int maxHealth = minion.getAttribute("MaxHealth");
        Float healthPercentage = ((float)health)/maxHealth;

        Float shieldPercentage = ((float)shield/2)/maxHealth;

        batch.draw(healthFill, healthBarPositionX, healthBarPositionY, healthBarWidth, healthBarHeight*healthPercentage);
        batch.draw(shieldFill, healthBarPositionX, healthBarPositionY, healthBarWidth, healthBarHeight*shieldPercentage);
        batch.draw(healthBar, healthBarPositionX, healthBarPositionY, healthBarWidth, healthBarHeight);
        //UIConstants.font.draw(batch, "" + health, getX(), getY());

        int attackStep = getStepFromAttack();

        Float attackWidth = getWidth() * 0.3f;
        Float attackPositionX = getX() + getWidth() - getWidth()*0.05f - attackWidth;
        Float attackPositionX2 = getX() + getWidth() - getWidth()*0.05f - 2*attackWidth - getWidth()*0.0f;
        Float attackPositionY = getY() + getHeight() - getWidth()*0.0f - attackWidth;

        if (!minion.isLeftPlayer)
        {
            attackPositionX = getX() + getWidth()*0.05f;
            attackPositionX2 = getX() + getWidth()*0.05f + attackWidth + getWidth()*0.00f;
        }

        for (int i = 0; i < attackStep; i++)
        {
            Texture t = sword;
            if (i % 2 != 0) t = swordFlipped;
            Float posX = attackPositionX;
            if (i > 1) posX = attackPositionX2;
            batch.draw(t, posX, attackPositionY, attackWidth, attackWidth);
        }

        batch.setColor(color.r, color.g, color.b, 1f);
    }

    int getStepFromAttack()
    {
        int step = 0;
        int attack = minion.getAttribute("AttackDmg");
        for (int i = 0; i < SliderType.Attack.steps(); i++)
        {
            if (attack > SliderType.Attack.valueForStep(i)) step++;
        }
        return step;
    }

    public void updateStats()
    {
        this.health = minion.getAttribute("Health");
        this.shield = minion.getAttribute("Shield");
    }

    public void setShield(int shield) {
        this.shield = shield;
    }
    public void setHealth(int health)
    {
        this.health = health;
    }

    @Override
    public String toString(){
        return "MinionNode at " +  minion.yPos + ", " + minion.xPos;
    }
}
