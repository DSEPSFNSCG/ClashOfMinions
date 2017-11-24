package com.clom.clashofminions.Nodes;

/**
 * Created by greensn on 10.11.17.
 */

public enum SliderType
{
    Health, Shield, Attack, HealingPower, AttackRange, HealBuffRange, AttackBoost, HealBuffBoost;

    public static final int numberOfTypes = 8;

    public int valueForStep(int step)
    {
        switch (this)
        {
            case Health: return 100 + step*50;
            case Shield: return step*50;
            case Attack: return step == 0 ? 10 : step*25;
            case HealingPower: return step*10;
            case AttackRange: return step;
            case HealBuffRange: return step;
            case AttackBoost: return step*10;
            case HealBuffBoost: return step*10;
        }
        return 0;
    }

    public int steps()
    {
        switch (this)
        {
            case Health: return 5;
            case Shield: return 5;
            case Attack: return 5;
            case HealingPower: return 5;
            case AttackRange: return 4;
            case HealBuffRange: return 4;
            case AttackBoost: return 5;
            case HealBuffBoost: return 5;
        }
        return 0;
    }

    public String iconName()
    {
        switch (this)
        {
            case Health: return "Icon-Heart.png";
            case Shield: return "Icon-Shield.png";
            case Attack: return "Icon-Sword-Crossed.png";
            case HealingPower: return "Icon-Healing.png";
            case AttackRange: return "Icon-AttackRange.png";
            case HealBuffRange: return "Icon-HealingRange.png";
            case AttackBoost: return "Icon-AttackBoost.png";
            case HealBuffBoost: return "Icon-HealingBoost.png";
        }
        return "";
    }

    @Override
    public String toString() {
        switch (this)
        {
            case Health: return "Health";
            case Shield: return "Shield";
            case Attack: return "AttackDmg";
            case HealingPower: return "Healing";
            case AttackRange: return "AttackRange";
            case HealBuffRange: return "BuffRange";
            case AttackBoost: return "AtkBuff";
            case HealBuffBoost: return "HealBuff";
        }
        return "";
    }
}
