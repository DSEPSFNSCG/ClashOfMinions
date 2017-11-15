package com.mygdx.game;

import com.badlogic.gdx.graphics.g2d.BitmapFont;

/**
 * Created by greensn on 09.11.17.
 */

public class UIConstants {
    public static final float menuButtonHeight = 0.15f;
    public static final float menuButtonWidth = 2.5668f * menuButtonHeight;

    public static final float gameLowerPadding = 0.03f;

    public static final float sliderGroupWidth = 0.75f;
    public static final float sliderGroupHeight = 0.28f;

    public static final float turnButtonPositionX = 1 - (0.1f + 1f/30f);
    public static final float turnButtonWidth = 0.1f;
    public static final float turnButtonHeight = sliderGroupHeight;

    public static final float manaBarPositionX = 0.8f;
    public static final float manaBarHeight = sliderGroupHeight;
    public static final float manaBarWidth = 0.05f;

    public static final int battleFieldTilesHorizontal = 10;
    public static final int battleFieldTilesVertical = 4;

    public static final float battleFieldHeight = 0.5f;
    public static final float battleFieldWidth = 0.95f;
    public static final float battleFieldPositionX = 0.025f;
    public static final float battleFieldPositionY = 0.42f;

    public static BitmapFont font;

}
