// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2009-2013 (http://safirsdkcore.com)
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

    /**
     * This Consumer Base class contains callback methods that can be overridden by an entity
     * handler that registers a handler for an entity type that can potentially be injected
     * outside the control of the handler itself.
     *
     * Examples of when an entity is injected outside the control of a handler includes
     * persistent data at startup time and entities received from other system installations
     * (multi-owned entities).
     *
     * The handler can reject the injection by invoking Delete for the received entity.
     *
     * In case the handler has no need to be informed when an entity is about to be injected
     * it can rely on the default implementation which just accepts the injected entity.
     *
     * An example of a situation where a handler needs to act on injected data is when the entity
     * has members that are references (EntityIds) to other entity instances, and the lifetime
     * of the pointed-to instance is completly connected to the lifetime of the referer.
     * If an entity has been injected it is the responsibility of the local handler to
     * traverse the entity instance graph, find changed references, and delete the
     * unreferenced entity instances. To support this task there are methods in the InjectedEntityProxy
     * class to read the previous entity state and find invalid refrences.
     *
     * Another example is when an injection relates only to some of the members (might be the case
     * when updates are received from external systems) and the handler wants to maintain some sort
     * of consistency between different members. In this case the handler can call
     * ConnectionAspectPostpone#IncompleteInjectionState to wait for more updates before the new state
     * is set.
     */
    public interface EntityInjectionBase extends EntityRequestBase
    {
        /**
         * Called when a new entity is about to be injected in the system.
         *
         * @param injectedEntityProxy Proxy object containing entity and meta information
         *                                   about the new entity that is about to be injected.
         */
        void onInjectedNewEntity(com.saabgroup.safir.dob.InjectedEntityProxy injectedEntityProxy);

        /**
         * Called when an updated entity is about to be injected in the system.
         *
         * @param injectedEntityProxy Proxy object containing entity and meta information
         *                                   about the updated entity that is about to be injected.
         */
        void onInjectedUpdatedEntity(com.saabgroup.safir.dob.InjectedEntityProxy injectedEntityProxy);

        /**
         * Called when an entity delete is about to be injected in the system.
         *
         * @param injectedEntityProxy Proxy object containing information about the entity about to
         *                                   be deleted.
         */
        void onInjectedDeletedEntity(com.saabgroup.safir.dob.InjectedEntityProxy injectedEntityProxy);

        /**
         * Indicates that all initial injection data has been transfered to the handler.
         *
         * In connection to the completion of an entity registration, the Dob will transfer
         * initial injection data to the handler. This method indicates that all such
         * data has been transfered, and thus, enables an effective handling of
         * the initial injection data by the handler.
         *
         * This method is guaranteed to be called once for each invocation of the following methods:
         * (This is true even when there is no initial injection data at all)
         * <ul>
         * <li>ConnectionBase#RegisterEntityHandlerInjection</li>
         * <li>ConnectionBase#RegisterEntityHandlerPending</li>
         * </ul>
         * @param typeId Type id in corresponding registration.
         * @param handlerId Handler id in corresponding registration.
         */
        void onInitialInjectionsDone(long typeId,
                                     com.saabgroup.safir.dob.typesystem.HandlerId handlerId);
    }

