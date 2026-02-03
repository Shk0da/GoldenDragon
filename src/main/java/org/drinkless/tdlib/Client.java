package org.drinkless.tdlib;

/**
 * Main class for interaction with the TDLib.
 */
public final class Client {

    /**
     * Interface for handler for results of queries to TDLib and incoming updates from TDLib.
     */
    public interface ResultHandler {
        /**
         * Callback called on result of query to TDLib or incoming update from TDLib.
         *
         * @param object Result of query or update of type TdApi.Update about new events.
         */
        void onResult(TdApi.Object object);
    }

    /**
     * Interface for handler of exceptions thrown while invoking ResultHandler.
     * By default, all such exceptions are ignored.
     * All exceptions thrown from ExceptionHandler are ignored.
     */
    public interface ExceptionHandler {
        /**
         * Callback called on exceptions thrown while invoking ResultHandler.
         *
         * @param e Exception thrown by ResultHandler.
         */
        void onException(Throwable e);
    }

    /**
     * Interface for handler of messages that are added to the internal TDLib log.
     */
    public interface LogMessageHandler {
        /**
         * Callback called on messages that are added to the internal TDLib log.
         *
         * @param verbosityLevel Log verbosity level with which the message was added from -1 up to 1024.
         *                       If 0, then TDLib will crash as soon as the callback returns.
         *                       None of the TDLib methods can be called from the callback.
         * @param message        The message added to the internal TDLib log.
         */
        void onLogMessage(int verbosityLevel, String message);
    }

    /**
     * Exception class thrown when TDLib error occurred while performing {@link #execute(TdApi.Function)}.
     */
    public static class ExecutionException extends Exception {
        /**
         * Original TDLib error occurred when performing one of the synchronous functions.
         */
        public final TdApi.Error error;

        /**
         * @param error TDLib error occurred while performing {@link #execute(TdApi.Function)}.
         */
        ExecutionException(TdApi.Error error) {
            super(error.code + ": " + error.message);
            this.error = error;
        }
    }

    /**
     * Sends a request to the TDLib.
     *
     * @param query            Object representing a query to the TDLib.
     * @param resultHandler    Result handler with onResult method which will be called with result
     *                         of the query or with TdApi.Error as parameter. If it is null, nothing
     *                         will be called.
     * @param exceptionHandler Exception handler with onException method which will be called on
     *                         exception thrown from resultHandler. If it is null, then
     *                         defaultExceptionHandler will be called.
     */
    public void send(TdApi.Function query, org.drinkless.tdlib.Client.ResultHandler resultHandler, org.drinkless.tdlib.Client.ExceptionHandler exceptionHandler) {
        long queryId = currentQueryId.incrementAndGet();
        if (resultHandler != null) {
            handlers.put(queryId, new org.drinkless.tdlib.Client.Handler(resultHandler, exceptionHandler));
        }
        nativeClientSend(nativeClientId, queryId, query);
    }

    /**
     * Sends a request to the TDLib with an empty ExceptionHandler.
     *
     * @param query         Object representing a query to the TDLib.
     * @param resultHandler Result handler with onResult method which will be called with result
     *                      of the query or with TdApi.Error as parameter. If it is null, then
     *                      defaultExceptionHandler will be called.
     */
    public void send(TdApi.Function query, org.drinkless.tdlib.Client.ResultHandler resultHandler) {
        send(query, resultHandler, null);
    }

    /**
     * Synchronously executes a TDLib request. Only a few marked accordingly requests can be executed synchronously.
     *
     * @param query Object representing a query to the TDLib.
     * @param <T>   Automatically deduced return type of the query.
     * @return request result.
     * @throws org.drinkless.tdlib.Client.ExecutionException if query execution fails.
     */
    @SuppressWarnings("unchecked")
    public static <T extends TdApi.Object> T execute(TdApi.Function<T> query) throws org.drinkless.tdlib.Client.ExecutionException {
        TdApi.Object object = nativeClientExecute(query);
        if (object instanceof TdApi.Error) {
            throw new org.drinkless.tdlib.Client.ExecutionException((TdApi.Error) object);
        }
        return (T) object;
    }

    /**
     * Creates new Client.
     *
     * @param updateHandler           Handler for incoming updates.
     * @param updateExceptionHandler  Handler for exceptions thrown from updateHandler. If it is null, exceptions will be ignored.
     * @param defaultExceptionHandler Default handler for exceptions thrown from all ResultHandler. If it is null, exceptions will be ignored.
     * @return created Client
     */
    public static org.drinkless.tdlib.Client create(org.drinkless.tdlib.Client.ResultHandler updateHandler, org.drinkless.tdlib.Client.ExceptionHandler updateExceptionHandler, org.drinkless.tdlib.Client.ExceptionHandler defaultExceptionHandler) {
        org.drinkless.tdlib.Client client = new org.drinkless.tdlib.Client(updateHandler, updateExceptionHandler, defaultExceptionHandler);
        synchronized (responseReceiver) {
            if (!responseReceiver.isRun) {
                responseReceiver.isRun = true;

                Thread receiverThread = new Thread(responseReceiver, "TDLib thread");
                receiverThread.setDaemon(true);
                receiverThread.start();
            }
        }
        return client;
    }

    /**
     * Sets the handler for messages that are added to the internal TDLib log.
     * None of the TDLib methods can be called from the callback.
     *
     * @param maxVerbosityLevel The maximum verbosity level of messages for which the callback will be called.
     * @param logMessageHandler Handler for messages that are added to the internal TDLib log. Pass null to remove the handler.
     */
    public static void setLogMessageHandler(int maxVerbosityLevel, org.drinkless.tdlib.Client.LogMessageHandler logMessageHandler) {
        nativeClientSetLogMessageHandler(maxVerbosityLevel, logMessageHandler);
    }

    private static class ResponseReceiver implements Runnable {
        public boolean isRun = false;

        @Override
        public void run() {
            while (true) {
                int resultN = nativeClientReceive(clientIds, eventIds, events, 100000.0 /*seconds*/);
                for (int i = 0; i < resultN; i++) {
                    processResult(clientIds[i], eventIds[i], events[i]);
                    events[i] = null;
                }
            }
        }

        private void processResult(int clientId, long id, TdApi.Object object) {
            boolean isClosed = false;
            if (id == 0 && object instanceof TdApi.UpdateAuthorizationState) {
                TdApi.AuthorizationState authorizationState = ((TdApi.UpdateAuthorizationState) object).authorizationState;
                if (authorizationState instanceof TdApi.AuthorizationStateClosed) {
                    isClosed = true;
                }
            }

            org.drinkless.tdlib.Client.Handler handler = id == 0 ? updateHandlers.get(clientId) : handlers.remove(id);
            if (handler != null) {
                try {
                    handler.resultHandler.onResult(object);
                } catch (Throwable cause) {
                    org.drinkless.tdlib.Client.ExceptionHandler exceptionHandler = handler.exceptionHandler;
                    if (exceptionHandler == null) {
                        exceptionHandler = defaultExceptionHandlers.get(clientId);
                    }
                    if (exceptionHandler != null) {
                        try {
                            exceptionHandler.onException(cause);
                        } catch (Throwable ignored) {
                        }
                    }
                }
            }

            if (isClosed) {
                updateHandlers.remove(clientId);           // there will be no more updates
                defaultExceptionHandlers.remove(clientId); // ignore further exceptions
                clientCount.decrementAndGet();
            }
        }

        private static final int MAX_EVENTS = 1000;
        private final int[] clientIds = new int[MAX_EVENTS];
        private final long[] eventIds = new long[MAX_EVENTS];
        private final TdApi.Object[] events = new TdApi.Object[MAX_EVENTS];
    }

    private final int nativeClientId;

    private static final java.util.concurrent.ConcurrentHashMap<Integer, org.drinkless.tdlib.Client.ExceptionHandler> defaultExceptionHandlers = new java.util.concurrent.ConcurrentHashMap<Integer, org.drinkless.tdlib.Client.ExceptionHandler>();
    private static final java.util.concurrent.ConcurrentHashMap<Integer, org.drinkless.tdlib.Client.Handler> updateHandlers = new java.util.concurrent.ConcurrentHashMap<Integer, org.drinkless.tdlib.Client.Handler>();
    private static final java.util.concurrent.ConcurrentHashMap<Long, org.drinkless.tdlib.Client.Handler> handlers = new java.util.concurrent.ConcurrentHashMap<Long, org.drinkless.tdlib.Client.Handler>();
    private static final java.util.concurrent.atomic.AtomicLong currentQueryId = new java.util.concurrent.atomic.AtomicLong();
    private static final java.util.concurrent.atomic.AtomicLong clientCount = new java.util.concurrent.atomic.AtomicLong();

    private static final org.drinkless.tdlib.Client.ResponseReceiver responseReceiver = new org.drinkless.tdlib.Client.ResponseReceiver();

    private static class Handler {
        final org.drinkless.tdlib.Client.ResultHandler resultHandler;
        final org.drinkless.tdlib.Client.ExceptionHandler exceptionHandler;

        Handler(org.drinkless.tdlib.Client.ResultHandler resultHandler, org.drinkless.tdlib.Client.ExceptionHandler exceptionHandler) {
            this.resultHandler = resultHandler;
            this.exceptionHandler = exceptionHandler;
        }
    }

    private Client(org.drinkless.tdlib.Client.ResultHandler updateHandler, org.drinkless.tdlib.Client.ExceptionHandler updateExceptionHandler, org.drinkless.tdlib.Client.ExceptionHandler defaultExceptionHandler) {
        clientCount.incrementAndGet();
        nativeClientId = createNativeClient();
        if (updateHandler != null) {
            updateHandlers.put(nativeClientId, new org.drinkless.tdlib.Client.Handler(updateHandler, updateExceptionHandler));
        }
        if (defaultExceptionHandler != null) {
            defaultExceptionHandlers.put(nativeClientId, defaultExceptionHandler);
        }
        send(new TdApi.GetOption("version"), null, null);
    }

    private static native int createNativeClient();

    private static native void nativeClientSend(int nativeClientId, long eventId, TdApi.Function function);

    private static native int nativeClientReceive(int[] clientIds, long[] eventIds, TdApi.Object[] events, double timeout);

    private static native TdApi.Object nativeClientExecute(TdApi.Function function);

    private static native void nativeClientSetLogMessageHandler(int maxVerbosityLevel, org.drinkless.tdlib.Client.LogMessageHandler logMessageHandler);
}
