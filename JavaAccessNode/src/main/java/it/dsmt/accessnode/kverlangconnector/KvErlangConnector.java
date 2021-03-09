package it.dsmt.accessnode.kverlangconnector;

import com.ericsson.otp.erlang.*;
import it.dsmt.accessnode.pojo.Response;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicLong;


/**
 * KvErlangConnector is responsible to offer a set of static functions to let the Java Access Node initiate all the
 * supported operations (by send requests to the Central Node) and complete them by receiving back the responses.
 * All methods are thread-safe except init() and close() that are performed only during the initialization phase /
 * exiting phase.
 */
public class KvErlangConnector {

    private static final String CENTRAL_NODE_PROCESS_REGISTERED_NAME = "central_node";
    private static final String CLIENT_NODE_MAILBOX_NAME = "access_node_mailbox";
    private static final Long TIMEOUT = 8000L;  //8 seconds
    private static boolean initialized = false;
    private static final ConcurrentHashMap<Long, Exchanger<String>> RECEIVED = new ConcurrentHashMap<>();
    private static String clientNodeName;  //"access_node@localhost";
    private static String centralNodeName;  //"centralnode@localhost";
    private static AtomicLong nextRequestId;
    private static Thread dispatchingThread;
    private static OtpNode clientNode;
    private static OtpMbox mailbox;


    private static void createAndStartDispatchingThread() {
        dispatchingThread = new Thread(() -> {
            while(!Thread.interrupted()) {
                OtpErlangTuple replyMsg;
                try {
                    replyMsg = (OtpErlangTuple)mailbox.receive();
                    if(replyMsg.elements().length != 2) {
                        System.err.println("Malformed Erlang message");
                        continue;
                    }
                } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
                    otpErlangExit.printStackTrace();
                    continue;
                }

                Long requestId;
                OtpErlangString reply;
                try {
                    requestId = ((OtpErlangLong) replyMsg.elementAt(0)).longValue();
                    reply = (OtpErlangString) replyMsg.elementAt(1);
                }
                catch (ClassCastException e) {
                    System.err.println("Malformed Erlang message");
                    continue;
                }

                try {
                    Exchanger<String> replyExchanger = RECEIVED.get(requestId);
                    if(replyExchanger != null) {
                        replyExchanger.exchange(reply.stringValue(), 0, TimeUnit.MILLISECONDS);
                    }
                }
                catch (InterruptedException | TimeoutException e) {
                    e.printStackTrace();
                    RECEIVED.remove(requestId);
                }
            }
        });
        dispatchingThread.start();
    }

    private static Response sendAndReceiveReply(OtpErlangObject obj) {
        //Get a requestId
        Long requestId = nextRequestId.incrementAndGet();

        //Create the Erlang message
        OtpErlangTuple erlMsg = new OtpErlangTuple(
                new OtpErlangObject[] {new OtpErlangLong(requestId), mailbox.self(), obj}
        );

        //Send out
        Exchanger<String> replyExchanger = new Exchanger<>();
        RECEIVED.put(requestId, replyExchanger);
        mailbox.send(CENTRAL_NODE_PROCESS_REGISTERED_NAME, centralNodeName, erlMsg);

        //Wait for reply
        String reply = "";
        try {
            reply = replyExchanger.exchange(reply, TIMEOUT, TimeUnit.MILLISECONDS);
            RECEIVED.remove(requestId);
        }
        catch (InterruptedException e) {
            e.printStackTrace();
            return Response.onlyStatus("Error: Internal server error");
        }
        catch (TimeoutException e) {
            e.printStackTrace();
            return Response.onlyStatus("Error: Erlang timeout exceeded");
        }

        return new Response("ok", reply);
    }


    /**
     * Initialize KvErlangConnector.
     * Must be called only once at webapp startup.
     */
    public static void init() {
        if(initialized) {
            return;
        }

        //Retrieve configuration from JSON file
        String accessNodeName;
        String myip;
        String centralNodeIp;
        String cookie;
        try (
                InputStream is = KvErlangConnector.class.getClassLoader().getResourceAsStream("/config/config.json");
                InputStreamReader isr = new InputStreamReader(is, StandardCharsets.UTF_8);
                BufferedReader reader = new BufferedReader(isr)
        ) {
            StringBuilder jsonString = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                jsonString.append(line).append("\n");
            }
            JSONObject jsonRoot = new JSONObject(jsonString.toString());
            accessNodeName = jsonRoot.getString("accessNodeName");
            myip = jsonRoot.getString("myip");
            centralNodeIp = jsonRoot.getString("centralNodeIp");
            cookie = jsonRoot.getString("cookie");
        } catch (IOException | JSONException | NullPointerException e) {
            e.printStackTrace();
            System.err.println("Access Node initialization failed");
            System.err.println("Check your config.json");
            return;
        }

        //Client and server node names
        clientNodeName = accessNodeName + "@" + myip;  //"access_node@localhost";
        centralNodeName = "centralnode@" + centralNodeIp;

        //Client node and mailbox
        try {
            OtpTransportFactory otptf = new OtpSocketTransportFactory();
            if (cookie == null || cookie.isEmpty())
                clientNode = new OtpNode(clientNodeName, otptf);
            else
                clientNode = new OtpNode(clientNodeName, cookie, otptf);
            mailbox = clientNode.createMbox(CLIENT_NODE_MAILBOX_NAME);
        }
        catch (IOException e) {
            System.err.printf(
                    e.getMessage() +
                    "%nAccess Node initialization failed%n" +
                    "Is the nameserver running?%n" +
                    "Try running erl -name whatever@IP%n" +
                    "Retry in seconds...%n"
            );
            //If the initialization fails, it must be repeated in seconds
            final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
            scheduler.schedule(KvErlangConnector::init, 15, TimeUnit.SECONDS);
            return;
        }
        System.out.printf(
                "JAN registered as %s, mailbox %s, cookie %s%n", clientNodeName, CLIENT_NODE_MAILBOX_NAME, cookie
        );
        System.out.printf(
                "JAN target is %s, process %s%n", centralNodeName, CENTRAL_NODE_PROCESS_REGISTERED_NAME
        );

        //Other static data member
        nextRequestId = new AtomicLong(0L);

        //Complete initialization by creating and starting the dispatchingThread
        createAndStartDispatchingThread();
        initialized = true;
    }

    /**
     * Stop and close KvErlangConnector.
     * Must be called when the webapp is closing.
     */
    public static void close() {
        if(!initialized)
            return;
        mailbox.close();
        clientNode.close();
        mailbox = null;
        clientNode = null;
        initialized = false;
        nextRequestId = null;
        dispatchingThread.interrupt();
        dispatchingThread = null;
        RECEIVED.clear();
    }

    /**
     * Check if KvErlangConnector is initialized.
     * @return true if initialized, otherwise false.
     */
    public static boolean getInitialized() {
        return initialized;
    }

    /**
     * Insert a key value-pair in the connected key-value storage.
     * @param key key of the key-value pair
     * @param value value of the key-value pair
     * @return the response
     */
    public static Response insertByKey(String key, String value) {
        //Create request
        OtpErlangAtom erlAtom = new OtpErlangAtom("insert");
        OtpErlangString erlKey = new OtpErlangString(key);
        OtpErlangString erlValue = new OtpErlangString(value);
        OtpErlangTuple erlMsg = new OtpErlangTuple(
                new OtpErlangObject[] {erlAtom, erlKey, erlValue}
        );

        return sendAndReceiveReply(erlMsg);
    }

    /**
     * Retrieve the value associated with the specified key from the key-value storage.
     * @param key key of the key-value pair
     * @return the response
     */
    public static Response getByKey(String key) {
        //Create request
        OtpErlangAtom erlAtom = new OtpErlangAtom("get");
        OtpErlangString erlKey = new OtpErlangString(key);
        OtpErlangTuple erlMsg = new OtpErlangTuple(
                new OtpErlangObject[] {erlAtom, erlKey}
        );

        return sendAndReceiveReply(erlMsg);
    }

    /**
     * Delete the key-value pair with the specified key in the key-value storage.
     * @param key key of the key-value pair
     * @return the response
     */
    public static Response deleteByKey(String key) {
        //Create request
        OtpErlangAtom erlAtom = new OtpErlangAtom("delete");
        OtpErlangString erlKey = new OtpErlangString(key);
        OtpErlangTuple erlMsg = new OtpErlangTuple(
                new OtpErlangObject[] {erlAtom, erlKey}
        );

        return sendAndReceiveReply(erlMsg);
    }

    public static String getClientNodeName() {
        return clientNodeName;
    }

}
