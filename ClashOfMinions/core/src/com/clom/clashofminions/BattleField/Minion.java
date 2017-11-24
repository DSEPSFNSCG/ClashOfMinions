package com.clom.clashofminions.BattleField;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by 2weirdy on 2017-11-10.
 */

public class Minion {
    public static final int numAttributes = 12;
    private static final HashMap<String,Integer> AttributeMap; // All values except for range should be direct values, not level values. I.E., Atk buff would be 10 and not 1.
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
        AttributeMap.put("BuffedAtk",8);
        AttributeMap.put("BuffedHealing",9);
        AttributeMap.put("MaxHealth", 10);
        AttributeMap.put("Shield", 11);
    }
    public static Map<String, Integer> getAttributeMap(){
        return Collections.unmodifiableMap(AttributeMap);
    }

    private final int[] attributes = new int[numAttributes];
    public final boolean isLeftPlayer;
    public int xPos;
    public int yPos;

    public Minion(boolean isLeftPlayer){
        this.isLeftPlayer = isLeftPlayer;
    }

    public boolean setAttribute(String s, int v){
        if(v<0) v = 0;
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
