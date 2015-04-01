/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef _SAFIR_DOB_CONNECTION_H
#define _SAFIR_DOB_CONNECTION_H

#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionBase.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/NodeStatus.h>

// Somewhat dirty but convenient for the Dob-user
#include <Safir/Dob/SecondaryConnection.h>

namespace Safir
{
namespace Dob
{
    /**
     * A connection to the DOB.
     *
     * This class represents a "real" (as opposed to SecondaryConnection) connection to the dob.
     * Each DOB application must have at least one connection. Connections are not thread safe.
     */
    class DOSE_CPP_API Connection : public ConnectionBase
    {
    public:

        /** Constructor.
         */
        Connection();

        /** Destructor.
         */
        virtual ~Connection();

        /**
         * Open a connection to the DOB.
         *
         * The connection uses the OnDoDispatch callback to signal that there is incoming data available.
         * When OnDoDispatch is called the application shall set an event or similar and then call
         * Dispatch() in this class from the thread that owns (has called Open)
         * the connection.
         *
         * There can be a number of contexts in the DOB. A connection is linked to the context specified in Open.
         * All operations using a connection is affecting only the context linked to that connection.
         * The intended primary usage is for recording/replay functionality. 0 is defined as the default
         * context.
         *
         * Note that connectionNameCommonPart together with connectionNameInstancePart must be unique
         * in the node.
         *
         * If NULL  is passed as the stopHandler argument the connection will not receive a stop order.
         * Normally only the main thread of an application should pass a non-NULL stopHandler, and it
         * should then tell other parts of the application to exit. If multiple stop handlers are specified
         * there is NO guaranteed order between which gets called first when a process receives a stop signal.
         *
         * @param [in] connectionNameCommonPart Name that identifies the program but not any particular
         *                                        program instance.
         * @param [in] connectionNameInstancePart Name that identifies a particular program instance.
         * @param [in] context Context
         * @param [in] stopHandler Object that implements the StopHandler interface.
         * @param [in] dispatcher Object that implements the Dispatcher interface.
         *
         * @throws Safir::Dob::NotOpenException The connection name is already used by someone else.
         *                                      Try another!
         */
        void Open(const std::wstring& connectionNameCommonPart,
                  const std::wstring& connectionNameInstancePart,
                  const Dob::Typesystem::Int32 context,
                  StopHandler * const stopHandler,
                  Dispatcher * const dispatcher);

        /**
         * Close the connection to the DOB.
         *
         * Closes the connection to the DOB and deallocates all resources. All subscriptions
         * and registrations will automatically be deleted and there is no need to call
         * Unsubscribe and Unregister before calling Close.
         * Note that all connections that were set up using Attach will also be closed after
         * a call to this method.
         */
        void Close();

        /**
         * Check if this Connection instance is open.
         *
         * @return True if the connection is open, otherwise false.
         */
        virtual bool IsOpen() const;

        /**
         * When the dispatch event or callback is signalled, the application MUST call
         * this method. A call to Dispatch will result in that all queues for this connection
         * are emptied and that each message in the queues are passed to the associated
         * consumer.
         * Calls to dispatch from connection instances that are not open will be ignored.
         */
        void Dispatch() const;

    private:

        //implementation of pure virtual
        virtual DotsC_Int32 GetControllerId() const {return m_ctrl;}

        DotsC_Int32 m_ctrl;

        //Disable copying and assignment
        Connection(const Connection&);
        Connection& operator=(const Connection&);
    };
}
}

#endif
