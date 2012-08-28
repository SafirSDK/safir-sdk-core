// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2006-2009 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
import com.saabgroup.dosetest.Action;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;

class Executor implements
        com.saabgroup.safir.dob.StopHandler,
        com.saabgroup.safir.dob.MessageSubscriber,
        com.saabgroup.safir.dob.EntityHandler,
        com.saabgroup.safir.dob.ServiceHandler {

    public Executor(String[] args) {
        m_instance = Integer.decode(args[0]);
        m_instanceString = Integer.toString(m_instance);
        m_partnerEntityId = new com.saabgroup.safir.dob.typesystem.EntityId(com.saabgroup.dosetest.Partner.ClassTypeId,
                new com.saabgroup.safir.dob.typesystem.InstanceId(m_instance));
        m_controlConnectionName = m_identifier + "_control";
        m_testConnectionName = "partner_test_connection";
        m_callbackActions = new java.util.EnumMap<com.saabgroup.safir.dob.CallbackId, java.util.Vector<com.saabgroup.dosetest.Action>>(com.saabgroup.safir.dob.CallbackId.class);
        for (com.saabgroup.safir.dob.CallbackId cb : com.saabgroup.safir.dob.CallbackId.values()) {
            m_callbackActions.put(cb, new java.util.Vector<com.saabgroup.dosetest.Action>());
        }

        m_controlDispatcher = new ControlDispatcher(m_synchronizer);
        m_testDispatcher = new Dispatcher(m_synchronizer);
        m_testStopHandler = new StopHandler();

        if (com.saabgroup.safir.dob.DistributionChannelParameters.getDistributionChannels(0).multicastAddress().getVal().equals("127.0.0.1")){
            System.out.println("System appears to be Standalone, not listening for multicasted test actions");
        }
        else {
            if (args.length > 1) {
                m_multicastNic = args[1];
            }

            m_actionReceiver = new ActionReceiver(m_synchronizer, m_multicastNic);
        }
    }

    public void run() throws InterruptedException {
        // Seems that subsequent garbage collections will execute faster after the first one so we start with
        // a GC here.
        System.gc();
        System.runFinalization();

        m_controlConnection.open(m_controlConnectionName, m_instanceString, 0, this, m_controlDispatcher);

        //subscribe to messages going to everyone and to me.
        m_controlConnection.subscribeMessage(com.saabgroup.dosetest.Action.ClassTypeId, new com.saabgroup.safir.dob.typesystem.ChannelId(m_instance), this);
        m_controlConnection.subscribeMessage(com.saabgroup.dosetest.Action.ClassTypeId, new com.saabgroup.safir.dob.typesystem.ChannelId(), this);

        if (m_actionReceiver != null) {
            m_actionReceiver.start();
        }

        System.out.println(m_identifier + ":" + m_instance + " Started");
        boolean DispatchControl;
        boolean DispatchTest;
        boolean StopOrder;
        boolean MC;

        while (!m_isDone) {
            synchronized (m_synchronizer) {
                //                    System.out.println("WAIT()");
                while (!m_synchronizer.DispatchControl &&
                       !m_synchronizer.DispatchTest &&
                       !m_synchronizer.StopOrder &&
                       !m_synchronizer.MC)
                {
                    m_synchronizer.wait();
                }
                DispatchControl = m_synchronizer.DispatchControl;
                DispatchTest = m_synchronizer.DispatchTest;
                StopOrder = m_synchronizer.StopOrder;
                MC = m_synchronizer.MC;

                m_synchronizer.DispatchControl = false;
                m_synchronizer.DispatchTest = false;
                m_synchronizer.StopOrder = false;
                m_synchronizer.MC = false;
            }

            //                System.out.println("DispatchControl = " + DispatchControl);
            //                System.out.println("MC = " + MC);
            //                System.out.println("DispatchTest = " + DispatchTest);
            //                System.out.println("StopOrder = " + StopOrder);

            if (DispatchControl) {

                try {
                    m_controlConnection.dispatch();
                } /* TODO:
                     catch (com.saabgroup.safir.dob.typesystem.Exception exc)
                     {
                     Logger.instance().println("Caught Exception when Dispatching controlConnection: " +
                     com.saabgroup.safir.dob.typesystem.Operations.getName(exc.getTypeId()));
                     System.out.println("Exception info: " + exc);
                     }*/ catch (com.saabgroup.safir.dob.typesystem.FundamentalException exc) {
                    Logger.instance().println("Caught FundamentalException when Dispatching controlConnection: "
                                              + com.saabgroup.safir.dob.typesystem.Operations.getName(exc.getTypeId()));
                    System.out.println("Exception info: " + exc);
                }

            }

            if (MC) {
                com.saabgroup.dosetest.Action action;
                while (!m_actionQueue.isEmpty()) {

                    synchronized (m_actionQueue) {
                        action = m_actionQueue.remove(0);
                    }
                    HandleAction(action);
                }


            }

            if (DispatchTest) {
                if (m_dispatchTestConnection && m_isActive) {
                    try {
                        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_DO_DISPATCH);
                        for (Consumer consumer : m_consumers) {
                            consumer.executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_DO_DISPATCH);
                        }

                        m_testConnection.dispatch();
                    } /*                            catch (com.saabgroup.safir.dob.typesystem.Exception exc)
                                                    {
                                                    Logger.instance().println("Caught Exception when Dispatching testConnection: " +
                                                    com.saabgroup.safir.dob.typesystem.Operations.getName(exc.getTypeId()));
                                                    System.out.println("Exception info: " + exc);
                                                    }*/ catch (com.saabgroup.safir.dob.typesystem.FundamentalException exc) {
                        Logger.instance().println("Caught FundamentalException when Dispatching testConnection: "
                                                  + com.saabgroup.safir.dob.typesystem.Operations.getName(exc.getTypeId()));
                        System.out.println("Exception info: " + exc);
                    }
                }
            }

            if (StopOrder) {
                Logger.instance().println("Got stop order");
                executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_STOP_ORDER);
                m_isDone = true;
            }
        }

        if (m_actionReceiver != null)
        {
            m_actionReceiver.interrupt();
            m_actionReceiver.join();
        }
    }

    private void executeAction(com.saabgroup.dosetest.Action action) {
        switch (action.actionKind().getVal()) {
            case ACTIVATE:
                if (action.identifier().getVal().equals(m_identifier)) {
                    m_defaultContext = action.context().getVal();
                    System.out.println("Activating (default context is " + m_defaultContext + ")");
                    if (!m_isActive) {
                        m_controlConnection.registerEntityHandler(m_partnerEntityId.getTypeId(),
                                new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance),
                                com.saabgroup.safir.dob.InstanceIdPolicy.HANDLER_DECIDES_INSTANCE_ID,
                                this);
                        m_controlConnection.registerServiceHandler(com.saabgroup.dosetest.Dump.ClassTypeId,
                                new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance), this);
                    }
                    com.saabgroup.dosetest.Partner partner = new com.saabgroup.dosetest.Partner();
                    partner.incarnation().setVal(0);
                    partner.identifier().setVal(m_identifier);
                    m_controlConnection.setAll(partner, m_partnerEntityId.getInstanceId(),
                            new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
                    m_isActive = true;
                }
                break;

            case DEACTIVATE:
                if (action.identifier().getVal().equals(m_identifier)) {
                    m_isActive = false;
                    System.out.println("Deactivating");
                    m_testConnection.close();
                    m_controlConnection.delete(m_partnerEntityId,
                            new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
                    m_controlConnection.unregisterHandler(m_partnerEntityId.getTypeId(),
                            new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
                    m_controlConnection.unregisterHandler(com.saabgroup.dosetest.Dump.ClassTypeId,
                            new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
                }
                break;

            case RESET:
                if (m_isActive) {
                    m_testConnection.close();

                    Dispatcher oldDispatcher = m_testDispatcher;  //keep this for a while, so we get a new dispatcher address.
                    m_testDispatcher = new Dispatcher(m_synchronizer);
                    oldDispatcher = null;

                    StopHandler oldStopHandler = m_testStopHandler;  //keep this for a while, so we get a new stopHandler address.
                    m_testStopHandler = new StopHandler();
                    if (oldStopHandler != null) {
                        oldStopHandler = null;
                    }

                    m_testConnection.open(m_testConnectionName, m_instanceString, m_defaultContext, null, m_testDispatcher);
                    try {
                        com.saabgroup.safir.dob.EntityProxy ep = m_controlConnection.read(m_partnerEntityId);
                        try {
                            com.saabgroup.dosetest.Partner partner = (com.saabgroup.dosetest.Partner) ep.getEntity();
                            partner.incarnation().setVal(partner.incarnation().getVal() + 1);
                            m_controlConnection.setChanges(partner, m_partnerEntityId.getInstanceId(),
                                    new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
                        } finally {
                            ep.dispose();
                        }
                    } catch (com.saabgroup.safir.dob.NotFoundException exc) {
                        System.out.println("WHAT?!");
                        System.exit(10);
                    }
                    Consumer[] oldCons = m_consumers; //keep these for a while, so we get new consumer addresses.
                    m_consumers = new Consumer[3];

                    for (int i = 0; i < 3; ++i) {
                        m_consumers[i] = new Consumer(i, m_testConnectionName, m_instanceString);
                    }

                    oldCons = null;

                    for (com.saabgroup.safir.dob.CallbackId callback : com.saabgroup.safir.dob.CallbackId.values()) {
                        m_callbackActions.get(callback).clear();
                    }
                }
                break;

            case CHECK_REFERENCES:
                if (m_isActive) {
                    if (m_consumers != null) {
                        m_consumers = null;
                    }

                    System.gc();
                    System.runFinalization();

                    // After releasing the executor's references and a garabage collection, there should be no
                    // Consumer instances
                    if (Consumer.instanceCount != 0) {
                        Logger.instance().println("Expected 0 consumer instances, but there is " + Consumer.instanceCount);
                    }

                    // restore consumers
                    m_consumers = new Consumer[3];
                    for (int i = 0; i < 3; ++i) {
                        m_consumers[i] = new Consumer(i, m_testConnectionName, m_instanceString);
                    }
                }
                break;

            case CLOSE_AND_CHECK_REFERENCES:
                if (m_isActive) {
                    m_testConnection.close();

                    if (m_consumers != null) {
                        m_consumers = null;
                    }

                    if (m_testDispatcher != null) {
                        m_testDispatcher = null;
                    }

                    if (m_testStopHandler != null) {
                        m_testStopHandler = null;
                    }

                    System.gc();
                    System.runFinalization();


                    // After releasing the executor's references and a garabage collection, there should be no
                    // Consumer instances and no Dispatcher instances
                    if (Consumer.instanceCount != 0) {
                        Logger.instance().println("Expected 0 consumer instances, but there is " + Consumer.instanceCount);
                    }
                    if (Dispatcher.instanceCount != 0) {
                        Logger.instance().println("Expected 0 dispatcher instances, but there is " + Dispatcher.instanceCount);
                    }
                    if (StopHandler.instanceCount != 0) {
                        Logger.instance().println("Expected 0 stopHandler instances, but there is " + StopHandler.instanceCount);
                    }

                    // Restore dispatcher
                    m_testDispatcher = new Dispatcher(m_synchronizer);

                    m_testConnection.open(m_testConnectionName, m_instanceString, 0, null, m_testDispatcher);

                    // Restore consumers
                    m_consumers = new Consumer[3];
                    for (int i = 0; i < 3; ++i) {
                        m_consumers[i] = new Consumer(i, m_testConnectionName, m_instanceString);
                    }

                    // Restore stopHandler
                    m_testStopHandler = new StopHandler();
                }
                break;

            case RUN_GARBAGE_COLLECTOR:
                if (m_isActive) {
                    System.gc();
                    System.runFinalization();
                }
                break;

            case OPEN: {
                if (m_isActive) {
                    int context = m_defaultContext;
                    if (!action.context().isNull()) {
                        context = action.context().getVal();
                    }

                    String connName = m_testConnectionName;
                    if (!action.connectionName().isNull()) {
                        connName = action.connectionName().getVal();
                    }

                    m_testConnection.open(connName,
                            m_instanceString,
                            context,
                            m_testStopHandler,
                            m_testDispatcher);
                }
            }
            break;

            case CLOSE: {
                if (m_isActive) {
                    m_testConnection.close();
                }
            }
            break;

            case INHIBIT_DISPATCH:
                if (m_isActive) {
                    m_dispatchTestConnection = !action.inhibit().getVal();
                    Logger.instance().println("InhibitDispatch set to " + m_dispatchTestConnection);
                }
                break;

            case INHIBIT_OUTGOING_TRAFFIC:
                if (m_isActive) {
                    boolean ok = com.saabgroup.safir.dob.test.util.Utilities.InhibitOutgoingTraffic(action.inhibit().getVal());


                    if (ok) {
                        Logger.instance().println("InhibitOutgoingTraffic set to "
                                + (com.saabgroup.safir.dob.test.util.Utilities.IsOutgoingTrafficInhibited() ? 1 : 0));
                    } else {
                        Logger.instance().println("InhibitOutgoingTraffic failed!");

                    }
                }
                break;


            case PRINT:
                if (m_isActive) {
                    Logger.instance().println(action.printString().getVal());
                }
                break;

            case RESET_CALLBACK_ACTIONS:
                for (com.saabgroup.safir.dob.CallbackId callback : com.saabgroup.safir.dob.CallbackId.values()) {
                    m_callbackActions.get(callback).clear();
                }
                break;

            case SLEEP: {
                if (m_isActive) {
                    System.out.println("Sleeping " + action.sleepDuration().getVal() + " seconds");
                    try {
                        Thread.sleep((int) (action.sleepDuration().getVal() * 1000.0));
                    } catch (InterruptedException e) {
                    }
                }
            }
            break;

            default:
                Logger.instance().println("Got unexpected action " + action.actionKind().getVal());
                break;
        }
    }

    void addCallbackAction(com.saabgroup.dosetest.Action action) {
        m_callbackActions.get(action.actionCallback().getVal()).add(action);
    }

    void executeCallbackActions(com.saabgroup.safir.dob.CallbackId callback) {
        for (com.saabgroup.dosetest.Action action : m_callbackActions.get(callback)) {
            executeAction(action);
        }
    }

    public void HandleAction(com.saabgroup.dosetest.Action action) {
        if (!action.partner().isNull() && (action.partner().getVal().getRawValue() != m_instance)) {
            // Not meant for this partner
            return;
        }

        if (action.consumer().isNull()) {//No consumer set, meant for the executor.
            if (action.actionCallback().isNull()) //it is a normal action
            {
                executeAction(action);
            } else if (m_isActive) {
                addCallbackAction(action);
            }
        } else if (m_isActive) {
            Consumer theConsumer = m_consumers[action.consumer().getVal()];

            if (action.actionCallback().isNull()) //it is a normal action
            {
                theConsumer.executeAction(action);
            } else {
                theConsumer.addCallbackAction(action);
            }
        }
    }

    //
    // StopHandler Members
    //
    @Override
    public void onStopOrder() {
        synchronized (m_synchronizer) {
            m_synchronizer.StopOrder = true;
            m_synchronizer.notify();
        }
    }

    //
    // MessageSubscriber Members
    //
    @Override
    public void onMessage(com.saabgroup.safir.dob.MessageProxy messageProxy) {
        executeCallbackActions(com.saabgroup.safir.dob.CallbackId.ON_MESSAGE);

        com.saabgroup.dosetest.Action action = (com.saabgroup.dosetest.Action) messageProxy.getMessage();

        HandleAction(action);
    }

    //
    // RevokedRegistrationBase Members
    //
    @Override
    public void onRevokedRegistration(long typeId, com.saabgroup.safir.dob.typesystem.HandlerId handlerId) {
        if (m_isActive) {
            Logger.instance().println("Deactivating");
            m_testConnection.close();
            m_controlConnection.unregisterHandler(m_partnerEntityId.getTypeId(), new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
            m_controlConnection.unregisterHandler(com.saabgroup.dosetest.Dump.ClassTypeId, new com.saabgroup.safir.dob.typesystem.HandlerId(m_instance));
            m_isActive = false;
        }
    }

    //
    // EntityRequestBase Members
    //
    @Override
    public void onCreateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender) {
        responseSender.send(new com.saabgroup.safir.dob.ErrorResponse());
    }

    @Override
    public void onDeleteRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender) {
        responseSender.send(new com.saabgroup.safir.dob.ErrorResponse());
    }

    @Override
    public void onUpdateRequest(com.saabgroup.safir.dob.EntityRequestProxy entityRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender) {
        responseSender.send(new com.saabgroup.safir.dob.ErrorResponse());
    }

    //
    // ServiceRequestBase Members
    //
    @Override
    public void onServiceRequest(com.saabgroup.safir.dob.ServiceRequestProxy serviceRequestProxy, com.saabgroup.safir.dob.ResponseSender responseSender) {
        com.saabgroup.dosetest.DumpResult result = new com.saabgroup.dosetest.DumpResult();
        result.result().setVal(Logger.instance().toString());
        Logger.instance().clear();
        responseSender.send(result);
    }

    private class Synchronizer {

        public Synchronizer() {
            DispatchControl = false;
            DispatchTest = false;
            StopOrder = false;
            MC = false;
        }
        public boolean DispatchControl;
        public boolean DispatchTest;
        public boolean StopOrder;
        public boolean MC;
    }

    //
    // ControlDispatcher subclass
    //
    private class ControlDispatcher implements com.saabgroup.safir.dob.Dispatcher {

        public ControlDispatcher(Synchronizer synchronizer) {
            m_synchronizer = synchronizer;
        }

        //
        // Dispatcher Members
        //
        @Override
        public void onDoDispatch() {
            synchronized (m_synchronizer) {
                m_synchronizer.DispatchControl = true;
                m_synchronizer.notify();
            }
        }
        private final Synchronizer m_synchronizer;
    }

    //
    // Dispatcher subclass
    //
    static private class Dispatcher implements com.saabgroup.safir.dob.Dispatcher {

        public static int instanceCount = 0;

        public Dispatcher(Synchronizer synchronizer) {
            //TODO            Interlocked.Increment(ref instanceCount);
            m_synchronizer = synchronizer;
        }

        //
        // Dispatcher Members
        //
        @Override
        public void onDoDispatch() {
            synchronized (m_synchronizer) {
                m_synchronizer.DispatchTest = true;
                m_synchronizer.notify();
            }
        }
        private final Synchronizer m_synchronizer;
        /* TODO:
        ~Dispatcher()
        {
        Interlocked.Decrement(ref instanceCount);
        }
         */
    }

    //
    // StopHandler subclass
    //
    static private class StopHandler implements com.saabgroup.safir.dob.StopHandler {

        public static int instanceCount = 0;

        public StopHandler() {
            /* TODO:
            Interlocked.Increment(ref instanceCount);
             */
        }

        /* TODO
        ~StopHandler()
        {
        Interlocked.Decrement(ref instanceCount);
        }
         */
        //
        // StopHandler Members
        //
        @Override
        public void onStopOrder() {
        }
    }

    //
    // MCSocket subclass
    //
    private class ActionReceiver extends Thread {

        public ActionReceiver(Synchronizer synchronizer, String multicastNic) {
            super();
            m_synchronizer = synchronizer;
            m_multicastNic = multicastNic;
            Init();
        }

        @Override
        public void interrupt() {
            super.interrupt();
            m_socket.close();
        }

        @Override
        public void run() {
            while(!isInterrupted()) {
                try {
                    m_socket.receive(m_recv);

                    byte[] data = m_recv.getData();
                    int length = m_recv.getLength();
                    int offset = m_recv.getOffset();
                    java.nio.ByteBuffer blob = java.nio.ByteBuffer.allocateDirect(length);
                    blob.clear();
  //                  System.out.println("DATA Length = " + length + " Offset = " + offset);
                    blob.put(data, offset, length);
                    com.saabgroup.safir.dob.typesystem.Object obj = com.saabgroup.safir.dob.typesystem.ObjectFactory.getInstance().createObject(blob);

//                    com.saabgroup.safir.dob.typesystem.BlobOperations.getTypeId(blob);
                    com.saabgroup.dosetest.Action action = (com.saabgroup.dosetest.Action)obj;

                    synchronized (m_actionQueue) {
                        m_actionQueue.add(action);
                    }

                } catch (IOException ex) {
                    if (!isInterrupted())
                    {
                        java.util.logging.Logger.getLogger(Executor.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }

                synchronized (m_synchronizer) {
                    m_synchronizer.MC = true;
                    m_synchronizer.notify();
                }

            }
        }

        private void Init() {
            try {
                InetAddress group = InetAddress.getByName(com.saabgroup.dosetest.Parameters.getTestMulticastAddress());

                m_socket = new MulticastSocket(port);

                if (m_multicastNic != null) {
                    System.out.println("Used NIC: " + m_multicastNic);
                    m_socket.setInterface(InetAddress.getByName(m_multicastNic));
                } else {
                    System.out.println("NIC is not set. Windows: Listen on all interfaces. Linux: Listen on default interface.");
                }

                m_socket.joinGroup(group);

                System.out.println("Joined socket to group for multicast reception. Multicast address " + group
                        + ", port " + port + ".");
            } catch (UnknownHostException ex) {
                java.util.logging.Logger.getLogger(Executor.class.getName()).log(Level.SEVERE, "Unknown host.", ex);
                java.lang.Runtime.getRuntime().exit(-1);
            } catch (IOException ex) {
                java.util.logging.Logger.getLogger(Executor.class.getName()).log(Level.SEVERE, "I/O exception", ex);
                java.lang.Runtime.getRuntime().exit(-1);
            }


        }
        private final Synchronizer m_synchronizer;
        private final int port = 31789;
        private MulticastSocket m_socket;
        private byte[] m_buf = new byte[65000];
        private DatagramPacket m_recv = new DatagramPacket(m_buf, m_buf.length);
    }
    //
    // Data members
    //
    private final String m_identifier = "java";
    private final int m_instance;
    private final String m_instanceString;
    private final String m_controlConnectionName;
    private final String m_testConnectionName;
    private final com.saabgroup.safir.dob.typesystem.EntityId m_partnerEntityId;
    private boolean m_isDone = false;
    private boolean m_isActive = false;
    private Consumer[] m_consumers;
    private int m_defaultContext = 0;
    private com.saabgroup.safir.dob.Connection m_controlConnection = new com.saabgroup.safir.dob.Connection();
    private com.saabgroup.safir.dob.Connection m_testConnection = new com.saabgroup.safir.dob.Connection();
    private boolean m_dispatchTestConnection = true;
    private final Synchronizer m_synchronizer = new Synchronizer();
    private ControlDispatcher m_controlDispatcher;
    private Dispatcher m_testDispatcher;
    private StopHandler m_testStopHandler;
    private ActionReceiver m_actionReceiver;
    private final List<Action> m_actionQueue = Collections.synchronizedList(new LinkedList<Action>());
    private String m_multicastNic = null;
    java.util.EnumMap<com.saabgroup.safir.dob.CallbackId, java.util.Vector<com.saabgroup.dosetest.Action>> m_callbackActions;
}
