package com.clom.clashofminions.Connection;

import com.badlogic.gdx.utils.Timer;
import com.sun.org.apache.xpath.internal.operations.Bool;

import java.io.IOException;
import java.net.InetAddress;

/**
 * Created by greensn on 04.12.17.
 */

public class AIConnectionHandler implements ConnectionHandler {

    ConnectionHandlerDelegate delegate;

    public AIConnectionHandler(InetAddress address, int port, String username) throws IOException {

    }

    @Override
    public void setDelegate(ConnectionHandlerDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public void searchGame() {

        final Boolean begins = (int)(Math.random() * 2) == 0;

        Timer.schedule(new Timer.Task(){
            @Override
            public void run() {
                if (delegate != null)
                {
                    delegate.gameFound("lol", 1, "Mallory", begins);
                }
            }
        }, 2.0f);

        if (!begins)
        {
            Timer.schedule(new Timer.Task(){
                @Override
                public void run() {
                    if (delegate != null)
                    {
                        delegate.receivedMove(9, (int)(Math.random() * 4), new int[]{3, 3, 3, 3, 3, 3, 3, 3});
                    }
                }
            }, 2.5f);
        }
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
                            }
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
                    if ((int)(Math.random() * 10) == 0)
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
