/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
*
* Created by: Anders Widén / stawi
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

#ifndef _SAFIR_DOB_MESSAGE_PROXY_H
#define _SAFIR_DOB_MESSAGE_PROXY_H

#include <Safir/Dob/Internal/ProxyImplPtr.h>
#include <Safir/Dob/DoseCppExportDefs.h>
#include <Safir/Dob/ConnectionInfo.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Defs.h>

#include <string>

//Undefine stupid macro that windows.h defines. Just in case someone has included it 
//before including us.
//TODO: Get rid of this, but when can we do it without potentially upsetting someone?
#ifdef GetMessage
#undef GetMessage
#endif

namespace Safir
{
namespace Dob
{

    // Forward declaration
    namespace Internal
    {
        class MessageProxyImpl;
    }

    /**
     * Proxy class for a message.
     */
    class DOSE_CPP_API MessageProxy
    {
    public:
        /**
         * Get type id.
         *
         * Retrieves type id of the message.
         *
         * @return Type id.
         */
        const Dob::Typesystem::TypeId GetTypeId() const;

        /**
         * Get message.
         *
         * Retrieves a smart pointer to the message.
         *
         * @return Message
         */
        const Dob::MessagePtr GetMessage() const;

        /**
         * Get info about the sender.
         *
         * Retrieves a smart pointer to info about the connection sending the message.
         *
         * @return Connection info.
         */
        const Dob::ConnectionInfoPtr GetSenderConnectionInfo() const;

        /**
         * Get channel id.
         *
         * Retrieves the channel on which the message is sent.
         *
         * @return Channel id.
         */
        const Dob::Typesystem::ChannelId GetChannelId() const;

        /**
         * Get binary blob of the received message.
         *
         * This method will give you a pointer to the underlying representation of the object.
         * Note that this pointer is only valid while the MessageProxy is in scope.
         * If you want to keep the blob you must copy it using methods in Safir::Dob::Typesystem.
         *
         * This method is mainly useful if all you want to do with a received object is to write it
         * to a database or pass it over a C-interface to a library or plugin.
         *
         * As an example, if you want to copy the bytes into a std::vector<char> you could do it
         * like this "v = std::vector<char>(blob,blob+Safir::Dob::Typesystem::BlobOperations.GetSize())"
         *
         * @return Binary blob of the received message.
         */
        const char * GetBlob() const;

        /**
         * @name Trace and Debug stuff
         */
        /** @{ */

        /**
         * Get channel id that also contains the string representation.
         *
         * Mainly for trace and debug purposes.
         * @see GetChannelId()
         *
         * @return Channel id.
         */
        const Dob::Typesystem::ChannelId GetChannelIdWithStringRepresentation() const;

        /** @} */

        // The constructor is for internal usage only!
        explicit MessageProxy(Internal::MessageProxyImpl* pImpl);

    private:

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4251) // To get rid of warning that says that the template needs to have a DLL-interface
#endif

        Internal::ProxyImplPtr<Internal::MessageProxyImpl> m_pImpl;

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    };
}
}

#endif
