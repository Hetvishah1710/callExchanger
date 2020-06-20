//--------------------------------------------------------
/*
 * Imports
 */
//--------------------------------------------------------
import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.logging.Logger;

//--------------------------------------------------------
/**
 * This is the Calling class which would start a thread
 * for every customer. Then the thread would use queue to
 * send data and receive request from another threads.
 */
//--------------------------------------------------------
public class Calling
{
    private static final URL path = ClassLoader.getSystemResource("calls.txt");
    private static final Logger logger = Logger.getLogger(Calling.class.getName());
    static ConcurrentHashMap<String, List<String>> callingMap = new ConcurrentHashMap<>();
    private static final Calling instance = new Calling();

    //--------------------------------------------------------
    /**
     * Constructor
     */
    //--------------------------------------------------------
    private Calling()
    {
        readCallingFile();
    }

    //--------------------------------------------------------
    /**
     * Singleton method to get instance
     */
    //--------------------------------------------------------
    public static Calling getInstance()
    {
        return instance;
    }

    //--------------------------------------------------------
    /**
     * Read calls file and store data into a hash map
     */
    //--------------------------------------------------------
    private void readCallingFile()
    {
        try
        {
            File file = new File(path.toURI());
            BufferedReader reader = new BufferedReader(new FileReader(file));
            String line;
            System.out.println("** Calls to be made ***");
            while ((line = reader.readLine()) != null)
            {
                String[] parts = line.split(",", 2);
                if (parts.length == 2)
                {
                    String key = parts[0].replace("{", "");
                    String value = parts[1].replace("[", "")
                            .replace("]", "")
                            .replaceAll("}.", "")
                            .replace(" ", "");
                    String[] str = value.split(",");
                    List<String> listValue = new LinkedList<>(Arrays.asList(str));
                    System.out.println(key + " " + listValue);
                    setCallingMap(key, listValue);
                }
            }
        }
        catch (URISyntaxException | IOException e)
        {
            e.printStackTrace();
        }
    }

    //--------------------------------------------------------
    /**
     * Setter to add data into the map
     */
    //--------------------------------------------------------
    public void setCallingMap(String key, List<String> value)
    {
        callingMap.put(key, value);
    }

    //--------------------------------------------------------
    /**
     * Function to display data into the console of all callers
     */
    //--------------------------------------------------------
    public void showCallingMap()
    {
        System.out.println("");
    }

    //--------------------------------------------------------
    /**
     * Process of every individual caller to send and receive
     * data from the other callers
     *
     * By using queues we can achieve this feat
     */
    //--------------------------------------------------------
    public static class CallingProcess implements Runnable
    {
        private String name;
        Timer timer = new Timer();
        ProcessTimer processTimer = null;

        //--------------------------------------------------------
        /**
         * Constructor
         */
        //--------------------------------------------------------
        CallingProcess(String name)
        {
            this.name = name;
            processTimer = new ProcessTimer(name);
        }

        @Override
        public void run()
        {
            boolean shouldRun = true;
            Random random = new Random();
            timer.schedule(processTimer, 7000);
            while (shouldRun == true)
            {
                if(callingMap.containsKey(name))
                {
                    if (Exchange.callingRequestQueue.size() < 1)
                    {
                        List<String> callingList = callingMap.get(name);
                        long time = System.currentTimeMillis();
                        Exchange.callingRequestQueue.add(name + " " + callingList.get(0) + " " + time);
                        callingList.remove(callingList.get(0));
                        if (callingList.size() == 0)
                        {
                            callingMap.remove(name);
                        }
                    }
                    try
                    {
                        Thread.sleep(random.nextInt(100));
                    }
                    catch (InterruptedException e)
                    {
                        e.printStackTrace();
                    }
                }

                if (Exchange.callingResponseQueue.size() > 0)
                {
                    if (Exchange.callingResponseQueue.peek() != null)
                    {
                        String[] response = Exchange.callingResponseQueue.peek().split(" ", 3);
                        if (name.equals(response[0]))
                        {
                            Exchange.getMessageOnMainThread(response[0], response[1], Exchange.ShowRequest.REPLY, ThreadLocalRandom.current().nextInt(1, 100), Long.parseLong(response[2]));
                            Exchange.callingResponseQueue.remove();
                        }
                        try
                        {
                            Thread.sleep(random.nextInt(100));
                        }
                        catch (InterruptedException e)
                        {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    //--------------------------------------------------------
    /**
     * Timer task to monitor the class time out
     */
    //--------------------------------------------------------
    public static class ProcessTimer extends TimerTask
    {
        String name;

        //--------------------------------------------------------
        /**
         * Constructor
         */
        //--------------------------------------------------------
        ProcessTimer(String name)
        {
            this.name = name;
        }

        @Override
        public void run()
        {
            Exchange.getMessageOnMainThread(name ,"", Exchange.ShowRequest.PROCESS_END, ThreadLocalRandom.current().nextInt(1, 100), 0);
        }
    }
}

//++++++++
// EOF
//++++++++