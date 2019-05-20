package com.spietras.picgallery.search.utils;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class ExecutorManager
{
    public static void shutdownExecutor(ExecutorService service, long timeout)
    {
        service.shutdown();
        try
        {
            if(!service.awaitTermination(timeout, TimeUnit.SECONDS))
            {
                service.shutdownNow();
            }
        }
        catch(InterruptedException e)
        {
            service.shutdownNow();
        }
    }

    public static void abortExecutor(ExecutorService service) throws InterruptedException, IllegalThreadStateException
    {
        service.shutdownNow();
        if(!service.awaitTermination(60L, TimeUnit.SECONDS))
            throw new IllegalThreadStateException("Some thread(s) still running");
    }
}
