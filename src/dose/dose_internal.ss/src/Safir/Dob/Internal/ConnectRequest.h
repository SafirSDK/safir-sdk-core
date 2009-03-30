/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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

#ifndef __DOSE_CONNECT_MESSAGE_H__
#define __DOSE_CONNECT_MESSAGE_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Internal/ConnectionId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct connect_tag_t {};
    struct disconnect_tag_t {};

    const connect_tag_t connect_tag = connect_tag_t();
    const disconnect_tag_t disconnect_tag = disconnect_tag_t();

    /** The possible results that can be in an ack. */
    enum ConnectResult
    {
        Undefined,
        Success,
        TooManyProcesses,
        TooManyConnectionsInProcess,
        ConnectionNameAlreadyExists,
    };


    class DOSE_INTERNAL_API ConnectRequest:
        public SharedMemoryObject
    {
    public:
        /** Constructor */
        ConnectRequest();

        void Set(connect_tag_t, const std::string & connectionName, const long context, const int pid);
        void Set(disconnect_tag_t, const ConnectionPtr & connection);

        void GetAndClear(connect_tag_t, std::string & connectionName, long & context, int & pid);
        void GetAndClear(disconnect_tag_t, ConnectionPtr & connection);

        bool IsConnect() const;

    private:
        enum Kind {NotSet, Connect, Disconnect};
        Kind m_kind;
        ShmString m_connectionName;
        long m_context;
        int m_pid;
        ConnectionPtr m_connection;
    };


    class DOSE_INTERNAL_API ConnectResponse:
        public SharedMemoryObject
    {
    public:
        /** Constructor */
        ConnectResponse();

        void Set(connect_tag_t, const ConnectResult result, const ConnectionPtr & connection);
        void Set(disconnect_tag_t, const ConnectResult result);

        void GetAndClear(connect_tag_t, ConnectResult & result, ConnectionPtr & connection);
        void GetAndClear(disconnect_tag_t, ConnectResult & result);

        //        bool IsConnect() const;

    private:
        enum Kind {NotSet, Connect, Disconnect};
        Kind m_kind;

        ConnectResult m_result;
        ConnectionPtr m_connection;
    };



    /*        enum ConnectRequestKind
        {
            NotSet,
            Connect,
            Disconnect,
            Ack
        };
    */



        /*
        //This can't be a union, since there is stuff in the structs that have constructors etc.
        //so we keep all the data always, but since there is only one instance of this thing allocated
        //in shared memory anyway, it doesnt really matter.

        struct
        {
            //            char m_connectionName[MAX_CONNECTION_NAME_LENGTH];
            ShmString m_connectionName;
            int m_pid;
        } ConnectData;

        struct
        {
            ConnectionId m_connectionId;
        } DisconnectData;

        struct
        {
            ShmString        m_connectionName;
            //            char             m_connectionName[MAX_CONNECTION_NAME_LENGTH];
            ConnectionResult m_result;
            ConnectionPtr    m_connection;
        } AckData;
        */

}
}
}

#endif

