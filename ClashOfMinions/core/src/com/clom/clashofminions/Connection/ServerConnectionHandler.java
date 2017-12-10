package com.clom.clashofminions.Connection;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Preferences;
import com.badlogic.gdx.utils.Timer;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;

/**
 * Created by 2weirdy on 2017-12-08.
 */

public class ServerConnectionHandler implements ConnectionHandler {
  private InetAddress address;
  private int port;
  private volatile ConnectionHandlerDelegate delegate;
  private InputStream iStream;
  private PrintWriter oStream;
  private String username;
  private boolean restoreFromMenu = true;

  private ReplyParser parser;
  private volatile boolean stop = false;
  Socket socket;

  /**
   * Reads a String from the opened connection. Do not call this from the main loop, as it may block.
   * @return a complete String read.
   * @throws IOException thrown by the inputstream reading operation.
   */
  public String readString() throws IOException {
    StringBuilder in = new StringBuilder();
    char c = (char) iStream.read();
    in.append(c);
    while (iStream.available() > 0 && c != '\n') {
      c = (char) iStream.read();
      in.append(c);
    }
    return in.toString();
  }

  /**
   * Basic constructor that also opens a connection with the server.
   * @param address IP address of the server
   * @param port port of the server
   * @param username Username the player wishes to register at the server
   * @throws IOException thrown by initializing the connection or the I/O streams.
   */
  public ServerConnectionHandler(InetAddress address, int port, String username) throws IOException{
    System.out.println("Starting connection");
    this.username = username;
    this.address = address;
    this.port = port;
    System.out.println(address.toString());
    System.out.println(port);
    socket = new Socket(address, port);
    iStream = socket.getInputStream();
    oStream = new PrintWriter(socket.getOutputStream());
    System.out.println("Established connection");
    parser = new ReplyParser();
    new Thread(parser).start();
  }

  /**
   * call to set the delegate
   * @param delegate delegate to be set
   */
  @Override
  public void setDelegate(ConnectionHandlerDelegate delegate) {
    this.delegate = delegate;
  }


  /**
   * call to start searching for the game. Usually is done immediately after construction. May be
   * called from any thread without danger, however cannot be safely called more than once.
   * Will also wait for the first move if the player is not starting first.
   * @throws IOException thrown by writing to the output stream.
   */
  @Override
  public void searchGame() throws IOException {
    restoreFromMenu = false;

    //Send message to server
    String msg = JSONbuilder.searchGame(username);
    System.out.println(msg);
    oStream.print(msg);
    oStream.flush();
    System.out.println("Wrote message");
  }

  /**
   * call to cancel searching for a game.
   */
  @Override
  public void cancelSearchingGame(){
    delegate = null;
    stop = true;
    try {
      iStream.close();
    }catch(IOException e){
    }
    oStream.close();
    try {
      socket.close();
    }catch(IOException e){
    }
  }


  /**
   * Call to attempt to restore the game
   * @param token
   * @param gameId
   * @param historyFrom the turn count from which we wish to rebuild the game state
   * @param fromStart true iff we wish to rebuild the game from scratch rather than rebuild any missing turns
   */
  @Override
  public void restoreGame(final String token, final int gameId, final int historyFrom, final boolean fromStart) {

    // send a message
    String msg = JSONbuilder.restoreGame(token, gameId,fromStart?0:delegate.historyStored());
    System.out.println(msg);
    oStream.print(msg);
    oStream.flush();
  }

  /**
   * Call to send a move to the server.
   * @param x
   * @param y
   * @param values
   */
  @Override
  public void sendMove(int x, int y, int[] values) {
    //Send movement to server
    String msg = JSONbuilder.sendMove(x,y,values);
    System.out.println(msg);
    oStream.print(msg);
    oStream.flush();
  }

  @Override
  public void quitGame(){
    delegate = null;
    stop = true;
    try {
      iStream.close();
    }catch(Exception e){
    }
    try{
      oStream.close();
    }catch(Exception e){
    }
    try{
      socket.close();
    }catch(Exception e){
    }
  }

  private static class JSONbuilder{
    public static String searchGame(String username){
      JSONObject o = new JSONObject();
      o.put("type","newGame");
      o.put("name",username);
      System.out.println(username);
      return o.toString() + "\n";
    }

    public static String sendMove(int x, int y, int[] values){
      JSONObject o = new JSONObject();
      o.put("type","place");

      JSONObject contents = new JSONObject();

      JSONArray position = new JSONArray();
      position.put(x);
      position.put(y);

      contents.put("position",position);

      JSONObject stats = new JSONObject();
      stats.put("maxhealth", values[0]);
      stats.put("shield", values[1]);
      stats.put("attackdmg", values[2]);
      stats.put("healing", values[3]);
      stats.put("attackrange", values[4]);
      stats.put("buffrange", values[5]);
      stats.put("atkbuff", values[6]);
      stats.put("healbuff", values[7]);

      contents.put("stats", stats);

      o.put("contents",contents);
      return o.toString() + "\n";
    }

    public static String restoreGame(String token, int gameId, int HistoryFrom){
      JSONObject o = new JSONObject();
      o.put("type","restoreGame");
      JSONObject contents = new JSONObject();
      contents.put("gameId",gameId);
      contents.put("token",token);
      contents.put("getHistoryFrom",HistoryFrom);
      o.put("contents",contents);
      return o.toString() + "\n";
    }
  }

  private class ReplyParser implements Runnable{
    public void run() {
      try {
        while (!stop) {
          String in;
          try {
            in = readString();
          } catch (IOException e) {
            continue;
          }
          System.out.println("received :" + in);
          JSONObject ans = new JSONObject(in);
          String type = ans.getString("type");
          if (type.equals("restoreSuccess")) {

            //read the message contents
            //TODO: only works from start as of now.
            JSONArray contents = ans.getJSONArray("contents");
            int size = contents.length();
            final int x[] = new int[size];
            final int y[] = new int[size];
            final int valuesArray[][] = new int[contents.length()][8];
            for (int i = 0; i < contents.length(); ++i) {
              JSONObject turn = contents.getJSONObject(i);
              JSONArray position = turn.getJSONArray("position");
              JSONObject stats = turn.getJSONObject("stats");
              int values[] = new int[8];
              values[0] = stats.getInt("maxhealth");
              values[1] = stats.getInt("shield");
              values[2] = stats.getInt("attackdmg");
              values[3] = stats.getInt("healing");
              values[4] = stats.getInt("attackrange");
              values[5] = stats.getInt("buffrange");
              values[6] = stats.getInt("atkbuff");
              values[7] = stats.getInt("healbuff");
              valuesArray[i] = values;
              x[i] = position.getInt(0);
              y[i] = position.getInt(1);
            }

            // and then restore the game on the main thread
            Timer.schedule(new Timer.Task() {
              @Override
              public void run() {
                if (delegate != null) {
                  delegate.restoredGame(x, y, valuesArray, true);
                }
              }
            }, 0.0f);
            break;
          } else if (type.equals("otherPlayerPlaced")) {
            JSONObject contents = ans.getJSONObject("contents");
            final JSONArray position = contents.getJSONArray("position");
            JSONObject stats = contents.getJSONObject("stats");
            final int values[] = new int[8];
            values[0] = stats.getInt("maxhealth");
            values[1] = stats.getInt("shield");
            values[2] = stats.getInt("attackdmg");
            values[3] = stats.getInt("healing");
            values[4] = stats.getInt("attackrange");
            values[5] = stats.getInt("buffrange");
            values[6] = stats.getInt("atkbuff");
            values[7] = stats.getInt("healbuff");

            // after receiving the information, tell the delegate to execute the received move on the main thread
            Timer.schedule(new Timer.Task() {
              @Override
              public void run() {
                if (delegate != null) {
                  delegate.receivedMove(position.getInt(0), position.getInt(1), values);
                }
              }
            }, 0.0f);
            break;
          } else if (type.equals("placeSuccess")) {
            Timer.schedule(new Timer.Task() {
              @Override
              public void run() {
                if (delegate != null) {
                  delegate.confirmMove();
                }
              }
            }, 0.0f);
          } else if (type.equals("invalidPlacing")) {
            Timer.schedule(new Timer.Task() {
              @Override
              public void run() {
                if (delegate != null) {
                  delegate.rejectMove();
                }
                Preferences preferences = Gdx.app.getPreferences("UserData");
                restoreGame(preferences.getString("token"), preferences.getInteger("gameId"), 0, true);
              }
            }, 0.0f);
          } else if (type.equals("gameStart")) {

            //get information about the game
            final String token = ans.getString("token");
            final String othername = ans.getString("otherName");
            final int gameID = ans.getInt("gameId");
            final boolean isFirstPlayer = ans.getBoolean("youStart");

            System.out.println(isFirstPlayer);
            //Schedule the delegate to arrange to start the game on the main loop
            Timer.schedule(new Timer.Task() {
              @Override
              public void run() {
                if (delegate != null) {
                  delegate.gameFound(token, gameID, othername, isFirstPlayer);
                }
              }
            }, 0.0f);
          } else if (type.equals("logResponse")) {
            System.out.println(ans.getString("contents"));
          } else if (type.equals("tokenMismatch")){
            if(restoreFromMenu){
              searchGame();
            }
          }
        }
      }catch(Exception e){
        e.printStackTrace();
        Timer.schedule(new Timer.Task() {
          @Override
          public void run() {
            if (delegate != null) {
              delegate.quitAction(); //TODO: display error to user?
            }
          }
        }, 0.0f);
      }
    }
  }
}
