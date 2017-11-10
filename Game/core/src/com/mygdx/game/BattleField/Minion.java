package com.mygdx.game.BattleField;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by 2weirdy on 2017-11-10.
 */

public class Minion {
    public static final int numAttributes = 8;
    private static final HashMap<String,Integer> AttributeMap;
    static{
        AttributeMap = new HashMap<String, Integer>();
        AttributeMap.put("Health",0);
        AttributeMap.put("AttackDmg", 1);
        AttributeMap.put("AttackRange",2);
        AttributeMap.put("BuffRange",3);
        AttributeMap.put("Healing",4);
        AttributeMap.put("AtkBuff",5);
        AttributeMap.put("HealBuff",6);
        AttributeMap.put("ShieldBuff",7);
    }
    public Map<String, Integer> getAttributeMap(){
        return Collections.unmodifiableMap(AttributeMap);
    }

    private final int[] attributes = new int[numAttributes];
    public final boolean isLeftPlayer;

    public Minion(boolean isLeftPlayer){
        this.isLeftPlayer = isLeftPlayer;
    }

    public boolean setAttribute(String s, int v){
        if(v<0) return false;
        Integer i = AttributeMap.get(s);
        if(i != null){
            attributes[i] = v;
            return true;
        }else{
            return false;
        }
    }

    public int getAttribute(String s){
        Integer i = AttributeMap.get(s);
        if(i != null){
            return attributes[i];
        }else{
            return -1;
        }
    }

}
