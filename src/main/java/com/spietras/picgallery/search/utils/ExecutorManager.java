package com.spietras.picgallery.search.utils;

import javafx.application.Platform;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class ExecutorManager
{
    /**
     * End ExecutorService peacefully
     *
     * @param service service to end
     * @param timeout time in seconds after we give up waiting
     */
    public static void shutdownExecutor(ExecutorService service, long timeout)
    {
        service.shutdown();
        try
        {
            if(!service.awaitTermination(timeout,
                                         TimeUnit.SECONDS)) //wait for all threads to terminate, if timeout abort and ignore
            {
                service.shutdownNow();
            }
        }
        catch(InterruptedException e)
        {
            service.shutdownNow();
        }
    }

    /**
     * End ExecutorService abruptly
     * @param service service to end
     * @throws InterruptedException when waiting was interrupted
     * @throws IllegalThreadStateException when waiting timed out, which means some threads are still running
     */
    public static void abortExecutor(ExecutorService service) throws InterruptedException, IllegalThreadStateException
    {
        service.shutdownNow();
        if(!service.awaitTermination(60L,
                                     TimeUnit.SECONDS)) //wait for all threads to terminate, if timeout throw exception
            throw new IllegalThreadStateException("Some thread(s) still running");
    }

    /**
     * Wait for all previous Runnables added to runLater to finish
     */
    public static void waitForRunLater() throws InterruptedException
    {
        Semaphore semaphore = new Semaphore(0);
        Platform.runLater(semaphore::release);
        semaphore.acquire();
    }
}
