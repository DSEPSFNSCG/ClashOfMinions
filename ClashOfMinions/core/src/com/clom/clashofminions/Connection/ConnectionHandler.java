package com.clom.clashofminions.Connection;

import java.io.IOException;

/**
 * Created by greensn on 04.12.17.
 */

public interface ConnectionHandler {

    void setDelegate(ConnectionHandlerDelegate delegate);

    void searchGame(String name) throws IOException;
    void cancelSearchingGame() throws IOException;

    void restoreGame(String token, int gameId);

    void sendMove(int x, int y, int[] values);

    void quitGame() throws IOException;

}
