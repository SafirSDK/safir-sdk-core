// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

package com.saabgroup.safir.dob;

final class Callbacks {

    /**
     * Helper function that just wraps asReadOnlyBuffer, but without choking
     * on null buffers.
     */
    private static java.nio.ByteBuffer makeReadOnly(java.nio.ByteBuffer buf){
        if (buf == null) {
            return null;
        }
        else {
            return buf.asReadOnlyBuffer();
        }
    }

    static void onDoDispatch(Dispatcher cons, boolean [] success)
    {
        success[0] = false;
        try
        {
            cons.onDoDispatch();
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onStopOrder(StopHandler cons, boolean [] success)
    {
        success[0] = false;
        try
        {
            cons.onStopOrder();
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onNewEntity(java.nio.ByteBuffer currentBlob,
                            java.nio.ByteBuffer currentState,
                            EntitySubscriber entitySubscriber,
                            boolean timestampDiff,
                            boolean [] success)
    {
        success[0] = false;
        try
        {
            EntityProxy proxy = new EntityProxy(makeReadOnly(currentBlob),
                                                makeReadOnly(currentState),
                                                null,
                                                null,
                                                true,
                                                timestampDiff);
            try {
                entitySubscriber.onNewEntity(proxy);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onUpdatedEntity(java.nio.ByteBuffer currentBlob,
                                java.nio.ByteBuffer currentState,
                                java.nio.ByteBuffer previousBlob,
                                java.nio.ByteBuffer previousState,
                                EntitySubscriber entitySubscriber,
                                boolean timestampDiff,
                                boolean [] success)
    {
        success[0] = false;
        try
        {
            EntityProxy proxy = new EntityProxy(makeReadOnly(currentBlob),
                                                makeReadOnly(currentState),
                                                makeReadOnly(previousBlob),
                                                makeReadOnly(previousState),
                                                true,
                                                timestampDiff);

            try {
                entitySubscriber.onUpdatedEntity(proxy);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onDeletedEntity(java.nio.ByteBuffer currentState,
                                java.nio.ByteBuffer previousBlob,
                                java.nio.ByteBuffer previousState,
                                boolean explicitlyDeleted,
                                EntitySubscriber entitySubscriber,
                                boolean timestampDiff,
                                boolean [] success)
    {
        success[0] = false;
        try
        {
            EntityProxy proxy = new EntityProxy(null,
                                                makeReadOnly(currentState),
                                                makeReadOnly(previousBlob),
                                                makeReadOnly(previousState),
                                                true,
                                                timestampDiff);
            try {
                entitySubscriber.onDeletedEntity(proxy,explicitlyDeleted);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onCreateRequest(java.nio.ByteBuffer blob,
                                java.nio.ByteBuffer state,
                                int ctrl,
                                int responseId,
                                EntityRequestBase entityRequestBase,
                                boolean [] success)
    {
        success[0] = false;
        try
        {
            ResponseSender rs = new ResponseSender(ctrl, entityRequestBase, responseId);
            EntityRequestProxy proxy = new EntityRequestProxy(makeReadOnly(blob),
                                                              makeReadOnly(state));

            try {
                entityRequestBase.onCreateRequest(proxy, rs);
            }
            finally {
                proxy.dispose();
            }
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onUpdateRequest(java.nio.ByteBuffer blob,
                                java.nio.ByteBuffer state,
                                int ctrl,
                                int responseId,
                                EntityRequestBase entityRequestBase,
                                boolean [] success)
    {
        success[0] = false;
        try
        {
            ResponseSender rs = new ResponseSender(ctrl, entityRequestBase, responseId);
            EntityRequestProxy proxy = new EntityRequestProxy(makeReadOnly(blob),
                                                              makeReadOnly(state));

            try {
                entityRequestBase.onUpdateRequest(proxy, rs);
            }
            finally {
                proxy.dispose();
            }
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onDeleteRequest(java.nio.ByteBuffer state,
                                int ctrl,
                                int responseId,
                                EntityRequestBase entityRequestBase,
                                boolean [] success)
    {
        success[0] = false;
        try
        {
            ResponseSender rs = new ResponseSender(ctrl, entityRequestBase, responseId);
            EntityRequestProxy proxy = new EntityRequestProxy(null, makeReadOnly(state));

            try{
                entityRequestBase.onDeleteRequest(proxy, rs);
            }
            finally {
                proxy.dispose();
            }
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }



    static void onServiceRequest(java.nio.ByteBuffer blob,
                                 java.nio.ByteBuffer state,
                                 int ctrl,
                                 int responseId,
                                 ServiceRequestBase consumer,
                                 boolean [] success)
    {
        success[0] = false;
        try
        {
            ResponseSender rs = new ResponseSender(ctrl, consumer, responseId);
            ServiceRequestProxy proxy = new ServiceRequestProxy(makeReadOnly(blob),
                                                                makeReadOnly(state));
            try {
                consumer.onServiceRequest(proxy, rs);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onResponse(int requestId,
                           java.nio.ByteBuffer responseBlob,
                           java.nio.ByteBuffer responseState,
                           java.nio.ByteBuffer requestBlob,
                           java.nio.ByteBuffer requestState,
                           Requestor requestor,
                           boolean [] success)
    {
        success[0] = false;
        try
        {
            ResponseProxy proxy = new ResponseProxy(requestId,
                                                    makeReadOnly(responseBlob),
                                                    makeReadOnly(responseState),
                                                    makeReadOnly(requestBlob),
                                                    makeReadOnly(requestState));
            try
            {
                requestor.onResponse(proxy);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onRevokedRegistration(long typeId,
                                      long handlerId,
                                      String handlerIdStr,
                                      RevokedRegistrationBase revokedRegistrationBase,
                                      boolean [] success)
    {
        success[0] = false;
        try
        {
            revokedRegistrationBase.onRevokedRegistration
                (typeId,new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId,handlerIdStr));
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }


    static void onCompletedRegistration(long typeId,
                                                long handlerId,
                                                String handlerIdStr,
                                                CompletedRegistrationBase completedRegistrationBase,
                                                boolean [] success)
    {
        success[0] = false;
        try
        {
            completedRegistrationBase.onCompletedRegistration
                (typeId,new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId,handlerIdStr));
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }


    static void onMessage(java.nio.ByteBuffer message,
                          java.nio.ByteBuffer state,
                          MessageSubscriber consumer,
                          boolean [] success)
    {
        success[0] = false;
        try
        {
            MessageProxy proxy = new MessageProxy(makeReadOnly(message),
                                                  makeReadOnly(state));
            try {
                consumer.onMessage(proxy);
            }
            finally{
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onRegistered(long typeId,
                             long handlerId,
                             String handlerIdStr,
                             RegistrationSubscriber registrationSubscriber,
                             boolean [] success)
    {
        success[0] = false;
        try
        {
            registrationSubscriber.onRegistered
                (typeId,new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId,handlerIdStr));
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onUnregistered(long typeId,
                               long handlerId,
                               String handlerIdStr,
                               RegistrationSubscriber registrationSubscriber,
                               boolean [] success)
    {
        success[0] = false;
        try
        {
            registrationSubscriber.onUnregistered
                (typeId,new com.saabgroup.safir.dob.typesystem.HandlerId(handlerId,handlerIdStr));
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onInjectedNewEntity(java.nio.ByteBuffer injectionBlob,
                                    java.nio.ByteBuffer injectionState,
                                    EntityInjectionBase entityInjectionBase,
                                    boolean [] success)
    {
        success[0] = false;
        try
        {
            InjectedEntityProxy proxy = new InjectedEntityProxy(makeReadOnly(injectionBlob),
                                                                makeReadOnly(injectionState),
                                                                null,
                                                                null);

            try {
                entityInjectionBase.onInjectedNewEntity(proxy);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onInjectedUpdatedEntity(java.nio.ByteBuffer injectionBlob,
                                        java.nio.ByteBuffer injectionState,
                                        java.nio.ByteBuffer currentBlob,
                                        java.nio.ByteBuffer currentState,
                                        EntityInjectionBase entityInjectionBase,
                                        boolean [] success)
    {
        success[0] = false;
        try
        {
            InjectedEntityProxy proxy = new InjectedEntityProxy(makeReadOnly(injectionBlob),
                                                                makeReadOnly(injectionState),
                                                                makeReadOnly(currentBlob),
                                                                makeReadOnly(currentState));

            try {
                entityInjectionBase.onInjectedUpdatedEntity(proxy);
            }
            finally {
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }

    static void onInjectedDeletedEntity(java.nio.ByteBuffer injectionState,
                                        java.nio.ByteBuffer currentBlob,
                                        java.nio.ByteBuffer currentState,
                                        EntityInjectionBase entityInjectionBase,
                                        boolean [] success)
    {
        success[0] = false;
        try
        {
            InjectedEntityProxy proxy = new InjectedEntityProxy(null,
                                                                makeReadOnly(injectionState),
                                                                makeReadOnly(currentBlob),
                                                                makeReadOnly(currentState));

            try {
                entityInjectionBase.onInjectedDeletedEntity(proxy);
            }
            finally{
                proxy.dispose();
            }

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }



    static void onInitialInjectionsDone(long typeId,
                                        long handlerId,
                                        String handlerIdStr,
                                        EntityInjectionBase entityInjectionBase,
                                        boolean [] success)
    {
        success[0] = false;
        try
        {
            entityInjectionBase.onInitialInjectionsDone
                (typeId,
                 new com.saabgroup.safir.dob.typesystem.HandlerId
                 (handlerId,handlerIdStr));

            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }


    static void onNotRequestOverflow(Requestor requestor, boolean [] success)
    {
        success[0] = false;
        try
        {
            requestor.onNotRequestOverflow();
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }


    static void onNotMessageOverflow(MessageSender messageSender, boolean [] success)
    {
        success[0] = false;
        try
        {
            messageSender.onNotMessageOverflow();
            success[0] = true;
        }
        catch (Exception exc)
        {
            com.saabgroup.safir.dob.typesystem.LibraryExceptions.getInstance().set(exc);
        }
    }
}

