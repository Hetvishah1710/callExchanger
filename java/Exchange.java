//--------------------------------------------------------
/*
 * Imports
 */
//--------------------------------------------------------
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.*;
import java.util.logging.Logger;


//--------------------------------------------------------
/**
 * This is the Exchange class which will handle all the
 * calls and requests from the given caller and will
 * display the output
 *
 * Thread would be using blocking queue to pass the messages
 * between them
 */
//--------------------------------------------------------
public class Exchange
{
    private static final Logger logger = Logger.getLogger(Exchange.class.getName());
    private static final Timer timer = new Timer();
    private static final Timer mainTimer = new Timer();
    static final PriorityBlockingQueue<String> callingRequestQueue  = new PriorityBlockingQueue<>();
    static final PriorityBlockingQueue<String> callingResponseQueue = new PriorityBlockingQueue<>();

    //--------------------------------------------------------
    /**
     *  Constructor
     */
    //--------------------------------------------------------
    private Exchange()
    {
        logger.info("Creating exchange constructor");
    }

    //--------------------------------------------------------
    /**
     * Enum to differentiate between different requests made
     */
    //--------------------------------------------------------
    public enum ShowRequest
    {
        INTRO,
        REPLY,
        PROCESS_END,
        MAIN_END
    }

    //--------------------------------------------------------
    /**
     * Main function of the program
     */
    //--------------------------------------------------------
    public static void main(String[] args)
    {
        Calling.getInstance().showCallingMap();
        ExecutorService executorService = Executors.newCachedThreadPool();

        try
        {
            TimeUnit.SECONDS.sleep(1);
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();
        }

        for (String key : Calling.callingMap.keySet())
        {
            Runnable callingWorker = new Calling.CallingProcess(key);
            executorService.execute(callingWorker);
        }

        try
        {
            TimeUnit.SECONDS.sleep(2);
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();
        }

        MainTimer mainTimerTask = new MainTimer();
        mainTimer.schedule(mainTimerTask, 10000);

        ProcessCallingExchange processCallingExchange = new ProcessCallingExchange();
        processCallingExchange.run();
    }

    //--------------------------------------------------------
    /**
     * Exchange process which would serve as the medium between
     * threads to check the response and pass the messages
     *
     * There are two queues
     * responseQueue
     * callingQueue
     */
    //--------------------------------------------------------
    public static class ProcessCallingExchange implements Runnable
    {
        boolean shouldRun = true;
        @Override
        public void run()
        {
            while (shouldRun == true)
            {
                if (callingRequestQueue.size() > 0)
                {
                    if (callingRequestQueue.peek() != null)
                    {
                        String[] callingRequestData = callingRequestQueue.peek().split(" ", 2);
                        getMessageOnMainThread(callingRequestData[0], callingRequestData[1], ShowRequest.INTRO, ThreadLocalRandom.current().nextInt(1, 100));
                        callingResponseQueue.add(callingRequestData[1] + " " + callingRequestData[0]);
                    }
                    callingRequestQueue.remove();
                }
                try {
                    Thread.sleep(200);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    //--------------------------------------------------------
    /**
     * The task to show the message on the console
     */
    //--------------------------------------------------------
    public static class DisplayMessage extends TimerTask
    {
        private String sender;
        private String receiver;
        private ShowRequest request;

        private DisplayMessage(String sender, String receiver, ShowRequest request)
        {
            this.sender   = sender;
            this.receiver = receiver;
            this.request  = request;
        }

        @Override
        public void run()
        {
            switch (request)
            {
                case INTRO:
                    System.out.println(receiver + " received intro message from " + sender + " [" + System.currentTimeMillis() + "]");
                    break;
                case REPLY:
                    System.out.println(receiver + " received reply message from " + sender + " [" + System.currentTimeMillis() + "]");
                    break;
                case PROCESS_END:
                    System.out.printf("%n Process " + sender + " has received no calls from 5 seconds, ending...%n");
                    break;
                case MAIN_END:
                    System.out.printf("%n Master has received no replies for 10 seconds, ending... %n");
                    System.exit(0);
            }
        }
    }

    //--------------------------------------------------------
    /**
     * Schedule task to print the output to console
     */
    //--------------------------------------------------------
    public static void getMessageOnMainThread(String sender, String receiver, ShowRequest request, int time)
    {
        DisplayMessage displayMessage = new DisplayMessage(sender, receiver, request);
        timer.schedule(displayMessage, time);
    }

    //--------------------------------------------------------
    /**
     * Schedule task to print the output to console
     */
    //--------------------------------------------------------
    private static class MainTimer extends TimerTask
    {
        MainTimer()
        {
            // empty constructor
        }

        @Override
        public void run()
        {
            getMessageOnMainThread(" "," ", ShowRequest.MAIN_END, 10);
        }
    }
}

//++++++++
// EOF
//++++++++
