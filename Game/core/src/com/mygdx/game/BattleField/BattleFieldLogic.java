package com.mygdx.game.BattleField;

import java.util.ArrayList;

/**
 * Created by 2weirdy on 2017-11-10.
 */

public class BattleFieldLogic {
    public final int width;
    public final int height;
    public final MinionNode[][] field;
    public boolean isLeftPlayerTurn = true;

    public final ArrayList<MinionNode> leftPlayerMinions = new ArrayList<MinionNode>();
    public final ArrayList<MinionNode> rightPlayerMinions = new ArrayList<MinionNode>();

    public BattleFieldLogic(int width, int height){
        this.width = width;
        this.height = height;
        field = new MinionNode[width][height];
    }

    public boolean addMinion(MinionNode m, int x, int y){
        if(field[x][y]==null) {
            field[x][y] = m;
            if(m.minion.isLeftPlayer){
                leftPlayerMinions.add(m);
            }else{
                rightPlayerMinions.add(m);
            }
            return true;
        }else{
            return false;
        }
    }

    public MinionNode getAtPos(int x, int y){
        return null;
    }

    public void doGameStep(){
        if(isLeftPlayerTurn){
            for(int x = width-1; x>=0; --x){
                for(int y = 0; y<height; --y){
                    if(field[x][y]!= null){

                    }
                }
            }
        }
    }

    public boolean addMinionAsTurn(MinionNode m, int x, int y, boolean isLeftPlayer){
        return false;
    }
}
