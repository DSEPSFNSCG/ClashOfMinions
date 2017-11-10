package com.mygdx.game.BattleField;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
 * Created by 2weirdy on 2017-11-10.
 */

public class BattleFieldLogic {
    public final int width;
    public final int height;
    public final MinionNode[][] field;
    public boolean isLeftPlayerTurn = true;
    public boolean gameOver = false;

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
            m.minion.xPos = x;
            m.minion.yPos = y;
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

    /**
     *
     * @return returns true if a player has won. Which player has won can be obtained from the field isLeftPlayerTurn.
     */
    public boolean doGameStep(){
        if(gameOver) return true;
        if(isLeftPlayerTurn){
            for(MinionNode n:leftPlayerMinions){
                Minion m = n.minion;
                if(field[m.xPos+1][m.yPos]==null){
                    ++m.xPos;
                    if(m.xPos == width-1){
                        gameOver = true;
                        return true;
                    }
                    field[m.xPos+1][m.yPos] = n;
                    field[m.xPos][m.yPos] = null;
                }
            }
            Collections.sort(leftPlayerMinions, new Comparator<MinionNode>() {
                @Override
                public int compare(MinionNode t0, MinionNode t1) {
                    if(t0.minion.xPos<t1.minion.xPos){
                        return -1;
                    }
                    if(t0.minion.xPos>t1.minion.xPos){
                        return 1;
                    }
                    return 0;
                }
            });
        }else{
            for(MinionNode n:rightPlayerMinions){
                Minion m = n.minion;
                if(field[m.xPos-1][m.yPos]==null){
                    --m.xPos;
                    if(m.xPos == 0){
                        gameOver = true;
                        return true;
                    }
                    field[m.xPos+1][m.yPos] = n;
                    field[m.xPos][m.yPos] = null;
                }
            }
            Collections.sort(rightPlayerMinions, new Comparator<MinionNode>() {
                @Override
                public int compare(MinionNode t0, MinionNode t1) {
                    if(t0.minion.xPos<t1.minion.xPos){
                        return 1;
                    }
                    if(t0.minion.xPos>t1.minion.xPos){
                        return -1;
                    }
                    return 0;
                }
            });
        }
    }

    public boolean addMinionAsTurn(MinionNode m, int x, int y, boolean isLeftPlayer){
        return false;
    }
}
