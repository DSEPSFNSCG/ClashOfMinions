package com.clom.clashofminions.Connection;

/**
 * Created by greensn on 04.12.17.
 */

public interface ConnectionHandlerDelegate {

    void gameFound(String token, String opponentName, Boolean isFirstPlayer);

    void receivedMove(int x, int y, int[] values);

    void opponentQuit();

    void restoredGame(int[] xs, int[] ys, int[][] valuesArray);
}
