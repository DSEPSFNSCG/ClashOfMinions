package com.clom.clashofminions.Connection;

import com.badlogic.gdx.utils.Timer;

/**
 * Created by greensn on 04.12.17.
 */

public class GameConnectionHandler implements ConnectionHandler {

    ConnectionHandlerDelegate delegate;

    @Override
    public void setDelegate(ConnectionHandlerDelegate delegate) {
        this.delegate = delegate;
    }

    @Override
    public void searchGame(String name) {

        Timer.schedule(new Timer.Task(){
            @Override
            public void run() {
                if (delegate != null)
                {
                    delegate.gameFound("lol", "Mallory", true);
                }
            }
        }, 2.0f);
    }

    @Override
    public void cancelSearchingGame() {
        delegate = null;
    }

    @Override
    public void restoreGame(String token) {

    }

    @Override
    public void sendMove(int x, int y, int[] values) {

        Timer.schedule(new Timer.Task(){
            @Override
            public void run() {
                if (delegate != null)
                {
                    delegate.receivedMove(9, (int)(Math.random() * 4), new int[]{1, 1, 1, 1, 1, 1, 1, 1});
                }
            }
        }, 1.0f);
    }

    @Override
    public void quitGame() {
        delegate = null;
    }
}
