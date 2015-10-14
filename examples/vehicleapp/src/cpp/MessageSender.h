/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Petter Lönnstedt / stpeln
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

#ifndef __MESSAGE_SENDER_H
#define __MESSAGE_SENDER_H

#include <boost/noncopyable.hpp>
#include <Safir/Dob/Connection.h> 

namespace VehicleAppCpp
{
    /** 
     * Defines a message to be sent. This class sends
     * the message.
     */
    class MessageSender :
        // Allows this class to send a message.
        public Safir::Dob::MessageSender,
        // Make this class a singleton. The reason is because
        // it's used from several classes in this application. 
        private boost::noncopyable
    {
    public:

        MessageSender();

        /** 
         * Returns the one and only instance to this singleton class.
         *
         * @return The instance.
         */
        static MessageSender& Instance();

        /** 
         * Initiates this class. Creates a secondary Dob
         * connection.
         */
        void Init();

        /** 
         * Methods derived from Safir::Dob::MessageSender.
         */
        void OnNotMessageOverflow();

        /** 
         * Sends a message.
         */
         void SendMaxNofVehicleMsg();

    private:
        // This class uses this secondary connection for Dob calls.
        Safir::Dob::SecondaryConnection m_connection;
    };
};
#endif
