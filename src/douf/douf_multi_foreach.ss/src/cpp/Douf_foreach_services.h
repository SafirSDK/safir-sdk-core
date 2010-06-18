/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Stefan Lindström / stsyli
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
#ifndef __DOUF_FOREACH_SERVICES_H
#define __DOUF_FOREACH_SERVICES_H

#include "Douf_foreach_data.h"

#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/ConnectionAspectMisc.h>
#include <Safir/Dob/ResponseSender.h> 
#include <Safir/Dob/ErrorListResponse.h>
#include <Safir/Application/Tracer.h>


#include <Safir/Application/Backdoor.h>
#include <Safir/Application/BackdoorKeeper.h>

#include <Safir/Utilities/ForEach/ResponseType.h>

#include <ace/Event_Handler.h>
#include <boost/date_time/posix_time/posix_time_duration.hpp>
#include <list>
#include <map>
#include <vector>


namespace Safir
{
    namespace Utilities
    {
        namespace ForEach
        {
            typedef std::vector<Safir::Utilities::ForEach::TransactionTableEntry> TransactionTable;
            typedef std::map<Safir::Dob::ResponseSenderPtr, Safir::Utilities::ForEach::RequestSpecificDataPtr> RequestMap;
            typedef std::vector<Safir::Utilities::ForEach::RequestSpecificDataPtr> RequestVector;
            /** 
            * Defines a service. This class handles the registration
            * of the service and processes requests.
            */
            class Services :
                // Allows this class to register as a service provider.
                public Safir::Dob::ServiceHandler,
                public Safir::Dob::Requestor,
#if NOT_YET
                public Safir::Application::Backdoor,
#endif
                public ACE_Event_Handler
            {
            public:

                /** Constructor
                */
                Services();

                /** Destructor
                */
                ~Services();

                /** 
                * Initiates this class. Creates a secondary DOB
                * connection and registeres the service.
                */
                void Init(const std::wstring& connectionNameCommonPart,
                          const std::wstring& connectionNameInstancePart);

                void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId     typeId,
                                           const Safir::Dob::Typesystem::HandlerId& handlerId);
     
                void OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                      Safir::Dob::ResponseSenderPtr   responseSender);


                void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
                void OnNotRequestOverflow(void);

                // Overrides Safir::Application::Backdoor, for more information see baseclass
                void HandleCommand(const std::vector<std::wstring>& cmdTokens);
                std::wstring GetHelpText(); 

                //Overrides ACE_Event_Handler, for more information see baseclass
                virtual int handle_timeout(const ACE_Time_Value & currentTime, const void * act);

            private:
                // This class uses this secondary connection for DOB calls.
                Safir::Dob::SecondaryConnection m_connection;
                
                // Tracer.
                Safir::Application::Tracer m_debug;

                // Stores data for each service request
                RequestMap m_requestData;

                // Send queue. Handles in which order outgoing request should be sent
                RequestVector m_sendQueue;

                // Sends an empty response
                void SendEmptyResponse(Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData,
                    Safir::Dob::ResponseSenderPtr replySender);

                // Send next request in queue for a specific service request
                bool SendNextRequest(Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData);

                // Schedule to send next request
                void ScheduleNextRequest(Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData, bool addToQueue);

                // Overflow setting for the backdoor
                bool m_backdoorOverflow;        
#if NOT_YET
                // Backdoor keeper, to handle program interface commands
                Safir::Application::BackdoorKeeper m_backdoorKeeper;
#endif
            };
        };
    }
}
#endif
