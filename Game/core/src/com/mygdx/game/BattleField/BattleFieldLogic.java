package com.mygdx.game.BattleField;

/**
 * Created by 2weirdy on 2017-11-10.
 */

public class BattleFieldLogic {
    public final int width;
    public final int height;
    public final MinionNode[][] field;
    public boolean isLeftPlayerTurn = true;

    public BattleFieldLogic(int width, int height){
        this.width = width;
        this.height = height;
        field = new MinionNode[width][height];
    }

    public boolean addMinion(MinionNode m, int x, int y){
        if(field[x][y]==null) {
            field[x][y] = m;
            return true;
        }else{
            return false;
        }
    }

    public MinionNode getAtPos(int x, int y){
        return null;
    }

    public void doGameStep(){

    }

    public boolean addMinionAsTurn(MinionNode m, int x, int y, boolean isLeftPlayer){
        return false;
    }
}
