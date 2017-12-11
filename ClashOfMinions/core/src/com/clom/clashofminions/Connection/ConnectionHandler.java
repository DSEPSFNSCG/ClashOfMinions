package com.clom.clashofminions.Connection;

import java.io.IOException;

/**
 * Created by greensn on 04.12.17.
 */

public interface ConnectionHandler {

    void setDelegate(ConnectionHandlerDelegate delegate);

    void searchGame() throws IOException;
    void cancelSearchingGame();

    void restoreGame(String token, int gameId, int historyFrom, boolean fromStart, String name);

    void sendMove(int x, int y, int[] values);

    void quitGame();

}
