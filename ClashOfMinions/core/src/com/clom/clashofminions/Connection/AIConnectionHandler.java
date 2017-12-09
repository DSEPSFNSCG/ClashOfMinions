package com.clom.clashofminions.Connection;

import com.badlogic.gdx.utils.Timer;

/**
 * Created by greensn on 04.12.17.
 */

public class AIConnectionHandler implements ConnectionHandler {

    ConnectionHandlerDelegate delegate;

    @Override
    public void setDelegate(ConnectionHandlerDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public void searchGame() {

        Timer.schedule(new Timer.Task(){
            @Override
            public void run() {
                if (delegate != null)
                {
                    delegate.gameFound("lol", 1, "Mallory", true);
                }
            }
        }, 2.0f);
    }

    @Override
    public void cancelSearchingGame() {
        delegate = null;
    }

    @Override
    public void restoreGame(String token, int gameId, int HistoryFrom, boolean fromStart) {
        Timer.schedule(new Timer.Task(){
            @Override
            public void run() {
                if (delegate != null)
                {
                    delegate.restoredGame(
                            new int[]{0, 9, 0, 9, 0, 9, 0, 9},
                            new int[]{0, 0, 1, 1, 2, 2, 3, 3},
                            new int[][]{
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                                    {3, 3, 3, 3, 3, 3, 3, 3},
                            },
                            true
                    );
                }
            }
        }, 2.0f);
    }

    @Override
    public void sendMove(int x, int y, int[] values) {

        Timer.schedule(new Timer.Task(){
            @Override
            public void run() {
                if (delegate != null)
                {
                    if ((int)(Math.random() * 2) == 0)
                    {
                        delegate.opponentQuit();
                        delegate = null;
                        return;
                    }

                    delegate.receivedMove(9, (int)(Math.random() * 4), new int[]{3, 3, 3, 3, 3, 3, 3, 3});
                }
            }
        }, 1.0f);
    }

    @Override
    public void quitGame() {
        delegate = null;
    }
}
