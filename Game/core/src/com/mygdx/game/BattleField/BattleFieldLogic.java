package com.mygdx.game.BattleField;

import java.lang.reflect.Array;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;

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

    public final ArrayList<MinionNode> movedMinions = new ArrayList<MinionNode>();
    public final HashMap<MinionNode, Integer> oldHealth = new HashMap<MinionNode, Integer>();
    public final ArrayList<MinionNode[]> minionBuffs = new ArrayList<MinionNode[]>();
    public final ArrayList<MinionNode[]> minionAttacks = new ArrayList<MinionNode[]>();
    public final ArrayList<MinionNode[]> minionHeals = new ArrayList<MinionNode[]>();

    /**
     * Basic Contructor
     * @param width width of the field in squares
     * @param height height of the field in squares
     */
    public BattleFieldLogic(int width, int height){
        this.width = width;
        this.height = height;
        field = new MinionNode[width][height];
    }

    /**
     * Fetch a Minion Node at a specific position
     * @param x
     * @param y
     * @return Minion node at the xy coordinate, or null if either the coordinate is invalid or no minion is there.
     */
    public MinionNode getMinionNode(int x, int y){
        if(width>x&&x>=0&&height>y&&y>=0){
            return field[x][y];
        }else{
            return null;
        }
    }

    /**
     * Used to add a minion with built in xy position.
     * @param m Minion to be added.
     */
    public void addMinion(MinionNode m){
        addMinion(m, m.minion.xPos, m.minion.yPos);
    }


    /**
     * Used to add a minion with custom xy position.
     * Will throw an Index out of bounds exception if the xy coordinate is invalid, and an illegal argument exception if there is already a minion at that position.
     * @param m Minion to be added
     * @param x x coordinate of the minion to be placed
     * @param y y coordinate of the minion to be placed
     */
    public void addMinion(MinionNode m, int x, int y){
        if(width>x&&x>=0&&height>y&&y>=0) {
            if (field[x][y] == null) {
                field[x][y] = m;
                m.minion.xPos = x;
                m.minion.yPos = y;
                if (m.minion.isLeftPlayer) {
                    leftPlayerMinions.add(m);
                } else {
                    rightPlayerMinions.add(m);
                }
                return;
            } else {
                throw new IllegalArgumentException();
            }
        }else{
            throw new IndexOutOfBoundsException();
        }
    }

    /**
     * Used to perform one turn of movement. Do not call this method.
     * @return returns true if the movement is a winning one.
     */
    public boolean doMovement(){
        ArrayList<MinionNode> curMinions = isLeftPlayerTurn ? leftPlayerMinions : rightPlayerMinions;
        final int xMod = isLeftPlayerTurn ? 1 : -1;
        for(MinionNode n:curMinions){
            Minion m = n.minion;
            if(field[m.xPos+xMod][m.yPos]==null){
                int  oldXpos = m.xPos;
                int newXPos = m.xPos + xMod;
                m.xPos = newXPos;
                field[newXPos][m.yPos] = n;
                field[oldXpos][m.yPos] = null;

                movedMinions.add(n);
                if((isLeftPlayerTurn&&m.xPos == width-1)||((!isLeftPlayerTurn)&&m.xPos==0)){
                    gameOver = true;
                    return true;
                }
            }
        }
        Collections.sort(curMinions, new Comparator<MinionNode>() {
            @Override
            public int compare(MinionNode t0, MinionNode t1) {
                if(t0.minion.xPos<t1.minion.xPos){
                    return -xMod;
                }
                if(t0.minion.xPos>t1.minion.xPos){
                    return xMod;
                }
                return 0;
            }
        });
        return false;
    }


    /**
     * Used to get the MinionNodes a certain minion can boost.
     * @param n The MinionNode doing the boosting
     * @return An arraylist containing the minions that can receive boosts.
     */
    public ArrayList<MinionNode> getInBoostRange(MinionNode n){
        Minion m = n.minion;
        ArrayList<MinionNode> buffTargets = new ArrayList<MinionNode>();
        MinionNode t;
        int range = n.minion.getAttribute("BuffRange");
        int xMod = n.minion.isLeftPlayer ? 1 : -1;
        switch (range) {
            case 3:
                t = getMinionNode(m.xPos - xMod, m.yPos - 1);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
                t = getMinionNode(m.xPos - xMod, m.yPos);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
                t = getMinionNode(m.xPos - xMod, m.yPos + 1);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
            case 2:
                t = getMinionNode(m.xPos + xMod, m.yPos - 1);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
                t = getMinionNode(m.xPos + xMod, m.yPos);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
                t = getMinionNode(m.xPos + xMod, m.yPos + 1);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
            case 1:
                t = getMinionNode(m.xPos, m.yPos - 1);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
                t = getMinionNode(m.xPos, m.yPos + 1);
                if (t != null) {
                    if(t.minion.isLeftPlayer == m.isLeftPlayer) buffTargets.add(t);
                }
                break;
            case 0:
            default:
                return null;
        }
        return buffTargets;
    }


    /**
     * Used to apply buffs. Do not call this method.
     */
    public void doBuffs(){
        ArrayList<MinionNode> curMinions = isLeftPlayerTurn ? leftPlayerMinions : rightPlayerMinions;
        for (MinionNode n : curMinions) {
            n.minion.setAttribute("BuffedHealing",n.minion.getAttribute("Healing"));
            n.minion.setAttribute("BuffedAtk",n.minion.getAttribute("AttackDmg"));
            Minion m = n.minion;
            int healbuff = m.getAttribute("HealBuff");
            int atkbuff = m.getAttribute("AtkBuff");
            if(healbuff + atkbuff <= 0) continue;
            ArrayList<MinionNode> buffTargets = getInBoostRange(n);
            if(buffTargets.size() == 0) continue;
            atkbuff = atkbuff/buffTargets.size();
            healbuff = (int)Math.ceil((double)healbuff/buffTargets.size());
            for (MinionNode target : buffTargets) {
                Minion targetm = target.minion;
                MinionNode[] pair = {n,target};
                minionBuffs.add(pair);
                targetm.setAttribute("BuffedAtk", targetm.getAttribute("BuffedAtk") + atkbuff);
                targetm.setAttribute("BuffedHealing", targetm.getAttribute("BuffedHealing") + healbuff);
            }
        }
        for (MinionNode n : curMinions) {
            Minion m = n.minion;
            int healing = m.getAttribute("BuffedHealing");
            if(healing <= 0) continue;
            ArrayList<MinionNode> buffTargets = getInBoostRange(n);
            if(buffTargets.size() == 0) continue;
            healing = (int)Math.ceil((double)healing/buffTargets.size());
            for (MinionNode target : buffTargets) {
                Minion targetm = target.minion;
                MinionNode[] pair = {n,target};
                minionHeals.add(pair);
                targetm.setAttribute("Health", targetm.getAttribute("Health") + healing);
            }
        }
    }


    /**
     * Used to perform attacks. Do not call this method.
     */
    public void doAttacks(){
        ArrayList<MinionNode> curMinions = isLeftPlayerTurn ? leftPlayerMinions : rightPlayerMinions;
        final int xMod = isLeftPlayerTurn ? 1 : -1;
        for(MinionNode n: curMinions){
            Minion m = n.minion;
            ArrayList<MinionNode> atkTargets = new ArrayList<MinionNode>();
            MinionNode t;
            switch(m.getAttribute("AttackRange")){
                case 3:
                    t = getMinionNode(m.xPos + xMod*2, m.yPos + 1);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                    t = getMinionNode(m.xPos + xMod*2, m.yPos - 1);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                case 2:
                    t = getMinionNode(m.xPos, m.yPos + 1);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                    t = getMinionNode(m.xPos, m.yPos - 1);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                case 1:
                    t = getMinionNode(m.xPos + xMod*2, m.yPos);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                    t = getMinionNode(m.xPos + xMod, m.yPos - 1);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                    t = getMinionNode(m.xPos + xMod, m.yPos + 1);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                case 0:
                    t = getMinionNode(m.xPos + xMod, m.yPos);
                    if (t != null) {
                        if(t.minion.isLeftPlayer != m.isLeftPlayer) atkTargets.add(t);
                    }
                    break;
                default:
                    continue;
            }

            int AD = m.getAttribute("BuffedAtk");
            if(atkTargets.size() == 0) continue;
            AD = AD/atkTargets.size();
            for (MinionNode targetn : atkTargets){
                Minion targetm = targetn.minion;
                targetm.setAttribute("Health",targetm.getAttribute("Health")-AD);
                MinionNode[] pair = {n,targetn};
                minionAttacks.add(pair);
                if(targetm.getAttribute("Health")==0){
                    leftPlayerMinions.remove(targetm);
                    rightPlayerMinions.remove(targetm);
                    field[targetm.xPos][targetm.yPos] = null;
                }
            }
        }
    }

    /**
     * Used to do one gamestep, regardless of whether a minion has been placed or not. Will switch turns. If a player has won, this method will do nothing.
     * @return returns true if a player has won. Which player has won can be obtained from the field isLeftPlayerTurn.
     */
    public boolean doGameStep(){
        if(gameOver) return true;

        movedMinions.clear();
        minionBuffs.clear();
        minionAttacks.clear();
        minionHeals.clear();
        ArrayList<MinionNode> otherMinions = isLeftPlayerTurn ? rightPlayerMinions : leftPlayerMinions;
        for(MinionNode n:otherMinions){
            oldHealth.put(n,n.minion.getAttribute("Health"));
        }

        if(doMovement()) return true;

        doBuffs();
        doAttacks();

        isLeftPlayerTurn = !isLeftPlayerTurn;
        return false;
    }

    /**
     * Preferred method for executing a turn. Adds the given minion node, then executes a turn if the player placing the minion is the one that should be taking a turn.
     * Will throw IndexOutOfBounds Exception in case of invalid coordinates, IllegalArgumentException if there is already a minion at the spot you're trying to place a new one,
     * and IllegalStateException if it's not the placing player's turn.
     * @param m The MinionNode to be placed. Attributes must already be allocated or undefined behavior will happen.
     * @param x
     * @param y
     * @param isLeftPlayer true if the player placing the minion is the one to the left.
     * @return returns true if a player has won after this turn, and false otherwise.
     */
    public boolean addMinionAsTurn(MinionNode m, int x, int y, boolean isLeftPlayer){
        if(isLeftPlayer != isLeftPlayerTurn || isLeftPlayer != m.minion.isLeftPlayer) throw new IllegalStateException("Not your turn you cheater!");
        addMinion(m,x,y);
        return doGameStep();
    }
}
