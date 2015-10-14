/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
#include "foreach_services.h"
#include "foreach_app.h"
#include "foreach_data.h"

#include <Safir/Dob/Response.h>
#include <Safir/Dob/SuccessResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>

#include <Safir/Utilities/ForEach/UpdateRequest.h>
#include <Safir/Utilities/ForEach/DeleteRequest.h>
#include <Safir/Utilities/ForEach/DeleteAllRequest.h>
#include <Safir/Utilities/ForEach/ResponseType.h>

#include <Safir/Utilities/ForEach/BriefResponse.h>
#include <Safir/Utilities/ForEach/FullResponse.h>

#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/RequestTimeoutProperty.h>
#include <Safir/Dob/NotFoundException.h>
#include <Safir/Logging/Log.h>
#include <Safir/Time/TimeProvider.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>

#include <Safir/Dob/Typesystem/Members.h>

namespace Safir
{
namespace Utilities
{
namespace ForEach
{
    Services::Services(boost::asio::io_service& ioService) :
        m_ioService(ioService),
        m_debug(L"Services")
    {
        //m_debug.Enable(true);
        // Send something to the tracer to open the connection.
        m_debug << " " << std::endl;
    }

    Services::~Services()
    {
#if NOT_YET
        // Stop backdoor keeper
        m_backdoorKeeper.Stop();
#endif
    }

    void Services::Init(const std::wstring& connectionNameCommonPart,
                        const std::wstring& connectionNameInstancePart)
    {
        m_debug << "**** Initializing Services ****"<<std::endl;
        m_connection.Attach(connectionNameCommonPart, connectionNameInstancePart);
        Safir::Dob::ConnectionAspectMisc connnectionAspectMisc(m_connection);

        // Register service.

        // Register as service provider.
        m_connection.RegisterServiceHandler(Safir::Utilities::ForEach::UpdateRequest::ClassTypeId, Safir::Dob::Typesystem::HandlerId(), this);
        m_connection.RegisterServiceHandler(Safir::Utilities::ForEach::DeleteRequest::ClassTypeId, Safir::Dob::Typesystem::HandlerId(), this);
        m_connection.RegisterServiceHandler(Safir::Utilities::ForEach::DeleteAllRequest::ClassTypeId, Safir::Dob::Typesystem::HandlerId(), this);
#if NOT_YET
        // Start backdoor keeper
        m_backdoorKeeper.Start(*this);
#endif
        /* to test overflow */
        m_backdoorOverflow = false;

        //m_connection.SetAlwaysOverflowFlag(m_backdoorOverflow);
        connnectionAspectMisc.SimulateOverflows(m_backdoorOverflow, m_backdoorOverflow);

        m_debug << "Force overflow is: " << m_backdoorOverflow << std::endl;
    }

    void Services::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                         const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        m_debug << "**** Services::OnRevokedRegistration() ****"<<std::endl;
    }

    void Services::OnServiceRequest(const Safir::Dob::ServiceRequestProxy serviceRequestProxy,
                                    Safir::Dob::ResponseSenderPtr replySender)
    {
        m_debug << "**** Services::OnServiceRequest() ****" << std::endl;

        /* data for an invalid request should not be stored */
        bool errorMessageSent = false;

        /* to store data about this request */
        boost::shared_ptr<RequestSpecificData> requestSpecificData(new RequestSpecificData);

        // Prepare error response
        Safir::Dob::ResponseErrorInfoPtr error = Safir::Dob::ResponseErrorInfo::Create();
        Safir::Dob::ErrorListResponsePtr errorList = Safir::Dob::ErrorListResponse::Create();
        // Create the common fields in the error list.
        errorList -> NumberOfErrors().SetVal(1);
        errorList -> Error()[0].SetPtr(error);

        // Handle service request.

        Safir::Dob::ServicePtr service = serviceRequestProxy.GetRequest();

        if (serviceRequestProxy.GetTypeId()== Safir::Utilities::ForEach::DeleteRequest::ClassTypeId)
        {

            /*****************************************************************************************
                 DELETE SERVICE
            *****************************************************************************************/


            // Cast to wanted service.
            const Safir::Utilities::ForEach::DeleteRequestPtr deleteService =
                boost::static_pointer_cast<Safir::Utilities::ForEach::DeleteRequest>(service);

            m_debug << "Delete request" << std::endl;

            requestSpecificData->GetRequestType() = Delete;

            /* if no responsetype is set, send an "SafirNullMember" error messsage back and don't do anything */
            if (deleteService->ResponseType().IsNull())
            {
                error -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirNullMember());
                error -> Member().SetVal(Safir::Utilities::ForEach::UpdateRequest::ResponseTypeMemberIndex());
                replySender->Send(errorList);
                errorMessageSent = true;
                m_debug << "ResponseType is NULL - sending error response" << std::endl;
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"ResponseType in Delete request is NULL - sending error response");
            }
            else
            {
                /* loop over array of ObjectsId, make a transaction entry and store it into requestSpecificData */
                for (std::vector<Safir::Dob::Typesystem::EntityIdContainer>::iterator it = deleteService->OperateOn().begin();
                     it != deleteService->OperateOn().end();
                     ++it)
                {
                    if (!it->IsNull())
                    {
                        requestSpecificData->NumberOfObjects()++;
                        Safir::Dob::Typesystem::EntityId objId = it->GetVal();

                        Safir::Utilities::ForEach::TransactionTableEntry entry;
                        entry.entityId = objId;
                        /* unknown requestId so far, indicates that no request has been done yet */
                        entry.requestId = -1;
                        requestSpecificData->Transactions().push_back(entry);

                        if (!errorMessageSent)
                        {
                            m_debug << "Inserting into map in for loop - " << requestSpecificData << std::endl;
                            /* insert into map */
                            m_requestData[replySender] = requestSpecificData;
                        }
                        else
                        {
                            m_debug << "Error message sent. NOT saving into map" << std::endl;
                        }
                    }

                    /* handle case of 0 objects. Normal reponse handling in OnResponse() will not be applicable.
                       send out response manually */
                    if (requestSpecificData->NumberOfObjects() == 0)
                    {
                        Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                      L"Delete reuest on 0 objects! Sending empty response.");
                        // save response type
                        requestSpecificData->ResponseType() = deleteService->ResponseType().GetVal();
                        SendEmptyResponse(requestSpecificData, replySender);
                        return;
                    }
                }

                if (deleteService->ResponseType().GetVal() == Safir::Utilities::ForEach::ResponseType::Immediate)
                {
                    m_debug << "ReponseType Immediate" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Dob::SuccessResponse::Create();
                    replySender -> Send(response);
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Immediate;
                }
                else if (deleteService->ResponseType().GetVal() == Safir::Utilities::ForEach::ResponseType::Brief)
                {
                    m_debug << "ReponseType Brief" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Utilities::ForEach::BriefResponse::Create();
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Brief;
                }
                else /* Full */
                {
                    m_debug << "ReponseType Full" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Utilities::ForEach::FullResponse::Create();
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Full;
                }

                /* send out next request in queue for this service request */
                ScheduleNextRequest(requestSpecificData, true);

            }
        }
        else if (serviceRequestProxy.GetTypeId()== Safir::Utilities::ForEach::DeleteAllRequest::ClassTypeId)
        {

            /*****************************************************************************************
                 DELETE ALL SERVICE
            *****************************************************************************************/

            const Safir::Utilities::ForEach::DeleteAllRequestPtr deleteAllService =
                boost::static_pointer_cast<Safir::Utilities::ForEach::DeleteAllRequest>(service);


            m_debug << "DeleteAll request" << std::endl;

            requestSpecificData->GetRequestType() = Delete;

            /* if no responsetype is set, send an "SafirNullMember" error messsage back and don't do anything */
            if (deleteAllService->ResponseType().IsNull())
            {
                error -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirNullMember());
                error -> Member().SetVal(Safir::Utilities::ForEach::DeleteAllRequest::ResponseTypeMemberIndex());
                replySender->Send(errorList);
                errorMessageSent = true;
                m_debug << "ResponseType is NULL - sending error response" << std::endl;
                Safir::Logging::SendSystemLog(Safir::Logging::Critical,
                                              L"ResponseType in DeleteAll request is NULL - sending error response");
            }
            else if (deleteAllService->TypeId().IsNull())
            {
                /* handle case of no typeId is specifed. Normal reponse handling in OnResponse() will not be applicable.
                   send out response manually */
                Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                              L"No typeId specified in DeleteAll request. Sending empty response.");
                // save response type
                requestSpecificData->ResponseType() = deleteAllService->ResponseType().GetVal();
                SendEmptyResponse(requestSpecificData, replySender);
                return;
            }
            else
            {

                bool includeSubClasses = true;
                if (!deleteAllService->IncludeSubClasses().IsNull())
                {
                    includeSubClasses = deleteAllService->IncludeSubClasses().GetVal();
                }
                m_debug << "Include subclasses is set to " << includeSubClasses << std::endl;

                Safir::Dob::Typesystem::HandlerId handlerId;
                bool handlerIdSet = false;
                if (!deleteAllService->HandlerId().IsNull())
                {
                    handlerId = deleteAllService->HandlerId().GetVal();
                    handlerIdSet = true;
                }

                /* loop over all instances of typeId and store into requestSpecifcData */
                // get iterator for this class
                for (Safir::Dob::EntityIterator it = m_connection.GetEntityIterator(deleteAllService->TypeId().GetVal(), includeSubClasses);
                     it != Safir::Dob::EntityIterator();
                     ++it)
                {
                    // if handlerId is set and does not match, skip this instance
                    if (handlerIdSet && it->GetOwner() != handlerId)
                    {
                        continue;
                    }

                    requestSpecificData->NumberOfObjects()++;
                    Safir::Dob::Typesystem::EntityId entityId = it->GetEntityId();
                    m_debug << "Added entityId to transaction list: " << it->GetEntityId().ToString() << std::endl;
                    Safir::Utilities::ForEach::TransactionTableEntry entry;
                    entry.entityId = entityId;
                    /* unknown requestId so far, indicates that no request has been done yet */
                    entry.requestId = -1;
                    requestSpecificData->Transactions().push_back(entry);
                }

                if (!errorMessageSent)
                {
                    m_debug << "Inserting into map in for loop - " << requestSpecificData << std::endl;
                    /* insert into map */
                    m_requestData[replySender] = requestSpecificData;
                }
                else
                {
                    m_debug << "Error message sent. NOT saving into map" << std::endl;
                }

                /* handle case of 0 objects. Normal reponse handling in OnResponse() will not be applicable.
                   send out response manually */
                if (requestSpecificData->NumberOfObjects() == 0)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Informational,
                                                  L"DeleteAll request operating on 0 objects. Sending empty response.");
                    // save response type
                    requestSpecificData->ResponseType() = deleteAllService->ResponseType().GetVal();
                    SendEmptyResponse(requestSpecificData, replySender);
                    return;
                }

                if (deleteAllService->ResponseType().GetVal() == Safir::Utilities::ForEach::ResponseType::Immediate)
                {
                    m_debug << "ReponseType Immediate" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Dob::SuccessResponse::Create();
                    replySender -> Send(response);
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Immediate;
                }
                else if (deleteAllService->ResponseType().GetVal() == Safir::Utilities::ForEach::ResponseType::Brief)
                {
                    m_debug << "ReponseType Brief" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Utilities::ForEach::BriefResponse::Create();
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Brief;
                }
                else /* Full */
                {
                    m_debug << "ReponseType Full" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Utilities::ForEach::FullResponse::Create();
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Full;
                }

                /* send out next request in queue for this service request */
                ScheduleNextRequest(requestSpecificData, true);

            }
        }
        else if (serviceRequestProxy.GetTypeId()== Safir::Utilities::ForEach::UpdateRequest::ClassTypeId)
        {

            /*****************************************************************************************
                 UPDATE SERVICE
            *****************************************************************************************/


            const Safir::Utilities::ForEach::UpdateRequestPtr updateService =
                boost::static_pointer_cast<Safir::Utilities::ForEach::UpdateRequest>(service);


            m_debug << "Update request";

            requestSpecificData->GetRequestType() = Update;

            /* if no responsetype is set, send an "SafirNullMember" error message back and don't do anything */
            if (updateService->ResponseType().IsNull())
            {
                error -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirNullMember());
                error -> Member().SetVal(Safir::Utilities::ForEach::UpdateRequest::ResponseTypeMemberIndex());
                replySender->Send(errorList);
                errorMessageSent = true;
                m_debug << "ResponseType is NULL - sending error response" << std::endl;
            }
            else if (updateService->TemplateEntityRequest().IsNull())
            {
                m_debug << "TemplateEntityRequest is NULL - handling as an error!" << std::endl;
                Safir::Logging::SendSystemLog(Safir::Logging::Error,
                                              L"TemplateEntityRequest in UpdateRequest is NULL!");

                error -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirNullMember());
                error -> Member().SetVal(Safir::Utilities::ForEach::UpdateRequest::TemplateEntityRequestMemberIndex());
                replySender->Send(errorList);
                errorMessageSent = true;
            }
            else
            {
                /* save the template entity request for later usage */
                requestSpecificData->TemplateEntityRequest() = updateService->TemplateEntityRequest().GetPtr();

                for (std::vector<Safir::Dob::Typesystem::EntityIdContainer>::iterator it = updateService->OperateOn().begin();
                     it != updateService->OperateOn().end();
                     ++it)
                {
                    if (!it->IsNull())
                    {
                        requestSpecificData->NumberOfObjects()++;
                        Safir::Dob::Typesystem::EntityId objId = it->GetVal();

                        Safir::Utilities::ForEach::TransactionTableEntry entry;
                        entry.entityId = objId;
                        entry.requestId = -1;
                        requestSpecificData->Transactions().push_back(entry);

                        if (!errorMessageSent)
                        {
                            m_debug << "Inserting into map in for loop - " << requestSpecificData << std::endl;
                            /* insert into map */
                            m_requestData[replySender] = requestSpecificData;
                        }
                        else
                        {
                            m_debug << "Error message sent - NOT saving into map!" << std::endl;
                        }
                    }
                }
                /* if request on an empty array of object ids */
                if (requestSpecificData->NumberOfObjects() == 0)
                {
                    Safir::Logging::SendSystemLog(Safir::Logging::Warning,
                                                  L"UpdateRequest Operating on 0 objects! Sending empty response.");
                    SendEmptyResponse(requestSpecificData, replySender);
                    return;
                }

                if (updateService->ResponseType().GetVal() == Safir::Utilities::ForEach::ResponseType::Immediate)
                {
                    m_debug << "ReponseType Immediate" << std::endl;
                    Safir::Dob::ResponsePtr response = Safir::Dob::SuccessResponse::Create();
                    // Send response.
                    replySender -> Send(response);
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Immediate;
                }
                else if (updateService->ResponseType().GetVal() == Safir::Utilities::ForEach::ResponseType::Brief)
                {
                    m_debug << "ReponseType Brief" << std::endl;
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Brief;
                }
                else /* Full */
                {
                    m_debug << "ReponseType Full" << std::endl;
                    requestSpecificData->ResponseType() = Safir::Utilities::ForEach::ResponseType::Full;
                }

                ScheduleNextRequest(requestSpecificData, true);

            }
        }
        else /* no service we know about */
        {
            m_debug << "before throw" << std::endl;
            /* what to do??? */
            throw;
        }



        /* for valid requests, store data into map */
        if (!errorMessageSent)
        {
            m_debug << "Inserting into map - " << requestSpecificData << std::endl;
            /* insert into map */
            m_requestData[replySender] = requestSpecificData;
        }
        else
        {
            m_debug << "Error message sent - NOT saving into map!" << std::endl;
        }
    }

    // Function:    SendEmptyResponse
    // Parameters:  Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData,
    //              Safir::Dob::ResponseSenderPtr replySender
    // Returns:     -
    // Comments:    Sends an empty response using replySender
    //
    void Services::SendEmptyResponse(Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData,
                                     Safir::Dob::ResponseSenderPtr replySender)
    {
        if (requestSpecificData->ResponseType() == Safir::Utilities::ForEach::ResponseType::Immediate)
        {
            m_debug << "ReponseType Immediate" << std::endl;
            Safir::Dob::ResponsePtr response = Safir::Dob::SuccessResponse::Create();
            replySender -> Send(response);
            m_debug << "Sent success response back" << std::endl;
        }
        else if (requestSpecificData->ResponseType() == Safir::Utilities::ForEach::ResponseType::Brief)
        {
            m_debug << "ReponseType Brief" << std::endl;
            Safir::Utilities::ForEach::BriefResponsePtr response = Safir::Utilities::ForEach::BriefResponse::Create();
            response->NumberOfErrorResponses().SetVal(0);
            response->NumberOfResponses().SetVal(0);
            response->NumberOfSuccessResponses().SetVal(0);
            replySender->Send(response);
            m_debug << "Sent Brief response back" << std::endl;
        }
        else /* Full */
        {
            m_debug << "ReponseType Full" << std::endl;
            Safir::Utilities::ForEach::FullResponsePtr response = Safir::Utilities::ForEach::FullResponse::Create();
            response->NumberOfErrorResponses().SetVal(0);
            response->NumberOfResponses().SetVal(0);
            response->NumberOfSuccessResponses().SetVal(0);
            replySender->Send(response);
            m_debug << "Sent Full response back" << std::endl;
        }
    }

    void Services::ScheduleNextRequest(Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData, bool addToQueue)
    {
        m_debug << "ScheduleNextRequest()" << std::endl;
        if (addToQueue)
        {
            // make sure this request will be handled
            m_sendQueue.push_back(requestSpecificData);
            m_debug << "added " << requestSpecificData << " to sendQueue" << std::endl;
        }
        else
        {
            m_debug << "without adding anything to the queue" << std::endl;
        }

        //make us send the requests once we've completed dispatching.
        m_ioService.post(boost::bind(&Services::SendQueuedRequests,this));
    }





    // Function:    SendNextRequest
    // Parameters:  Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData,
    // Returns:     bool - if it was possible to send out a request or not
    // Comments:    Send next request in queue to entity owner
    //
    bool Services::SendNextRequest(Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData)
    {
        Safir::Dob::RequestId reqId = -1;
        bool requestToSend = false;

        m_debug << "SendNextRequest" << std::endl;

        for (TransactionTable::iterator it = requestSpecificData->Transactions().begin();
             it != requestSpecificData->Transactions().end();
             ++it)
        {
            m_debug << "looping element ";
            Safir::Utilities::ForEach::TransactionTableEntry &entry = *it;
            if (entry.requestId == -1)
            {
                try
                {
                    if (requestSpecificData->GetRequestType() == Delete)
                    {
                        requestToSend = true;
                        reqId = m_connection.DeleteRequest(entry.entityId, this);
                        m_debug << "DeleteRequest on type " << Safir::Dob::Typesystem::Operations::GetName(entry.entityId.GetTypeId())
                                << " instanceId: " << entry.entityId.GetInstanceId().ToString() << std::endl;
                    }
                    else /* UPDATE_REQUEST */
                    {
                        m_debug << "update request" << std::endl;

                        Safir::Dob::EntityPtr templateObject;

                        if (requestSpecificData->TemplateEntityRequest() != NULL)
                        {
                            m_debug << "Entry object is of type: " << Safir::Dob::Typesystem::Operations::GetName(entry.entityId.GetTypeId()) << std::endl;
                            m_debug << "TER is of type: " << Safir::Dob::Typesystem::Operations::GetName(requestSpecificData->TemplateEntityRequest()->GetTypeId()) << std::endl;

                            if ((Safir::Dob::Typesystem::Operations::IsOfType(requestSpecificData->TemplateEntityRequest()->GetTypeId(),
                                                                              entry.entityId.GetTypeId()) && (Safir::Dob::Typesystem::Operations::IsOfType(entry.entityId.GetTypeId(),
                                                                                                                                                           requestSpecificData->TemplateEntityRequest()->GetTypeId()))))
                            {
                                m_debug << "TER and entry object is of the exactly same type" << std::endl;

                                requestToSend = true;
                                templateObject = requestSpecificData->TemplateEntityRequest();
                                //templateObject->SetInstanceNumber(entry.entityId.GetInstance());
                                reqId = m_connection.UpdateRequest(templateObject, entry.entityId.GetInstanceId(), this);
                                m_debug << "UpdateRequest on type " << Safir::Dob::Typesystem::Operations::GetName(templateObject->GetTypeId()) << std::endl;

                            }
                            else
                            {
                                m_debug << "No relation between objects" << std::endl;
                            }

                        }
                        else /* we don't have a TemplateEntityRequest object */
                        {
                            // should never come here (checked in OnRequest()
                        }

                    }
                    m_debug << "Request id: " << reqId << std::endl;

                    /* Copy the request id */
                    entry.requestId = reqId;
                    requestSpecificData->SetCurrentTransaction(&entry);
                    m_debug << "Set CurrentTransaction" << std::endl;
                    // requestSpecificData->CurrentTransaction() = (TransactionTableEntry *) &entry;
                }
                catch (const Safir::Dob::OverflowException &)
                {
                    m_debug << "We got overflow... Add requestSpecificData to sendQueue for later retry" << std::endl;
                    m_sendQueue.push_back(requestSpecificData);
                }
                catch (const Safir::Dob::NotFoundException &e)
                {
                    //It's allowed to update a non-existing object. This exception will not be thrown for that reason.
                    m_debug << "Couldn't find object in DOB! - " << e.GetExceptionInfo() << std::endl;
                }
                m_debug << "Only send out one request. Breaking out!" << std::endl;
                break;
            }
        }

        m_debug << "DEBUG - transactions" << std::endl;
        for (TransactionTable::iterator it = requestSpecificData->Transactions().begin();
             it != requestSpecificData->Transactions().end();
             ++it)
        {
            Safir::Utilities::ForEach::TransactionTableEntry entry = (*it);
            m_debug << "entry.requestId: " << entry.requestId << std::endl;
        }

        return requestToSend;
    }



    /* incoming responses from our call to m_connection.Delete/UpdateRequest() */
    void Services::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
    {
        m_debug << " in OnResponse() - requestId: " << responseProxy.GetRequestId() << std::endl;

        for (RequestMap::iterator iter = m_requestData.begin(); iter != m_requestData.end(); ++iter)
        {
            Safir::Dob::ResponseSenderPtr replySender = iter->first;
            RequestSpecificDataPtr data = iter->second;
            bool foundRequest = false;

            if (data->CurrentTransaction() == 0)
            {
                m_debug << "CurrentTransaction is NULL" << std::endl;
            }
            else
            {
                Safir::Utilities::ForEach::TransactionTableEntry &entry = (*data->CurrentTransaction());
                m_debug << "CurrentTransaction is set. requestId is " << entry.requestId << std::endl;
                if (entry.requestId == responseProxy.GetRequestId())
                {
                    foundRequest = true;
                    m_debug << entry.entityId << " with requestId " << entry.requestId << std::endl;
                    /* save response for later use */
                    entry.responsePtr = responseProxy.GetResponse();

                    /* keep track on number of responses we got */
                    data->NumberOfResponses()++;
                    m_debug << "Got " << data->NumberOfResponses() << " number of responses so far." <<
                        " Expecting " << data->NumberOfObjects() << std::endl;

                    Safir::Dob::SuccessResponsePtr tmp = boost::dynamic_pointer_cast<Safir::Dob::SuccessResponse>(responseProxy.GetResponse());
                    if (tmp != NULL)
                    {
                        m_debug << "classified as a successresponse" << std::endl;
                        data->NumberOfSuccessResponses()++;
                    }
                    else
                    {
                        m_debug << "classified as an errorresponse" << std::endl;
                        data->NumberOfErrorResponses()++;
                    }
                }
            }

            if (foundRequest && (data->NumberOfResponses() == data->NumberOfObjects()))
            {
                m_debug << "We got all " << data->NumberOfResponses() << " and are now ready to send a response back!" << std::endl;

                /* get map's key to retrieve replySender */
                Safir::Dob::ResponseSenderPtr replySender = iter->first;

                if (data->ResponseType() == Safir::Utilities::ForEach::ResponseType::Brief)
                {
                    Safir::Utilities::ForEach::BriefResponsePtr response = Safir::Utilities::ForEach::BriefResponse::Create();
                    /* fill in response fields with info */
                    response->NumberOfErrorResponses().SetVal(data->NumberOfErrorResponses());
                    response->NumberOfResponses().SetVal(data->NumberOfResponses());
                    response->NumberOfSuccessResponses().SetVal(data->NumberOfSuccessResponses());
                    replySender->Send(response);
                    m_debug << "Sent Brief response back" << std::endl;
                }
                else if (data->ResponseType() == Safir::Utilities::ForEach::ResponseType::Full)
                {
                    Safir::Utilities::ForEach::FullResponsePtr response = Safir::Utilities::ForEach::FullResponse::Create();
                    response->NumberOfErrorResponses().SetVal(data->NumberOfErrorResponses());
                    response->NumberOfResponses().SetVal(data->NumberOfResponses());
                    response->NumberOfSuccessResponses().SetVal(data->NumberOfSuccessResponses());
                    /* fill in array of responses */

                    int i = 0;
                    for (TransactionTable::iterator it = data->Transactions().begin();
                         it != data->Transactions().end();
                         ++it)
                    {
                        Safir::Utilities::ForEach::TransactionTableEntry entry = (*it);
                        m_debug << "entityId: " << entry.entityId << " requestId: " << entry.requestId <<
                            " responsePtr: " << entry.responsePtr << std::endl;

                        /* because of ResponseContainerArray we can't use insert/delete */
                        response->Response()[i++].SetPtr(entry.responsePtr);
                    }

                    replySender->Send(response);
                    m_debug << "Sent Full response back" << std::endl;

                }
                else if (data->ResponseType() == Safir::Utilities::ForEach::ResponseType::Immediate)
                {
                    m_debug << "We have requested an Immediate response type... "
                            << "So just ignoring the response and not sending anything back (already done!)" << std::endl;
                }
                else /* e.g. NULL, should not happen */
                {
                    Safir::Dob::ResponsePtr response = Safir::Dob::Response::Create();
                    Safir::Dob::ResponseErrorInfoPtr error = Safir::Dob::ResponseErrorInfo::Create();
                    Safir::Dob::ErrorListResponsePtr errorList = Safir::Dob::ErrorListResponse::Create();

                    errorList -> NumberOfErrors().SetVal(1);
                    errorList -> Error()[0].SetPtr(error);
                    error -> Code().SetVal(Safir::Dob::ResponseGeneralErrorCodes::SafirNullMember());
                    replySender->Send(errorList);

                    m_debug << "ResponseType is NULL - sending error response. Shouldn't happen!" << std::endl;
                }

                /* in order to process a new service request we need to cleanup */
                m_debug << "Cleaning up" << std::endl;
                m_requestData.erase(iter->first);

                // we are finished, no more work to do.
                m_debug << "Sending next request (if any in queue)......." << std::endl;
                // schedule, but don't add anything to the queue!!!!
                ScheduleNextRequest(data, false);
                break;
            }
            else if (data->NumberOfResponses() > data->NumberOfObjects())
            {
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"Programming error: Received more responses than expected", __WFILE__, __LINE__);
            }

            if (foundRequest) /* if we already found a request and still in for loop, we probably have outstanding requests to send out */
            {
                m_debug << " Sending next request......." << std::endl;
                ScheduleNextRequest(data, true);
                m_debug << "Breaking out.. Only send one request. Must wait for response." << std::endl;
                break;
            }
            else
            {
                m_debug << "Haven't found request. Looking further..." << std::endl;
            }
        }
    }

    /* Problems testing Overflow so a bit messy */
    void Services::OnNotRequestOverflow()
    {
        m_debug << "in OnNotRequestOverflow()" << std::endl;
        m_debug << "overflow is set to: " << m_backdoorOverflow << std::endl;

        RequestMap::iterator iter;
        iter = m_requestData.begin();
        Safir::Dob::ResponseSenderPtr replySender = iter->first;
        RequestSpecificDataPtr data = iter->second;

        ScheduleNextRequest(data, true);
    }


    void Services::SendQueuedRequests()
    {
        m_debug << "**** Received timeout event ****" << std::endl;

        m_debug << "Queue size: " << m_sendQueue.size() << std::endl;

        /* debug */
        for (int i = 0; i < (signed) m_sendQueue.size(); i++)
        {
            Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData = m_sendQueue[i];
            int no = requestSpecificData->ResponseType();
            m_debug << "sendQueue element " << i << " has address " << m_sendQueue[i] << std::endl;
            m_debug << "sendQueue contains a request of response type " << no << std::endl;
        }

        if (!m_sendQueue.empty())
        {
            // take out first element and delete it from the queue
            Safir::Utilities::ForEach::RequestSpecificDataPtr requestSpecificData = m_sendQueue[0];
            m_sendQueue.erase(m_sendQueue.begin());

            m_debug << "sendQueue not empty - processing a request containing " << requestSpecificData->NumberOfObjects()
                    << " no of objects" << std::endl;
            SendNextRequest(requestSpecificData);
        }
        else
        {
            m_debug << "sendQueue EMPTY!" << std::endl;
            /* not needed. Will fail if another ongoing service request will end, and request a ScheduleNextRequest(asd, false)
               and add a timer without something in sendQueue. This branch will be selected and we will have more than one outstanding
               request */
        }
    }



    // Function:    HandleCommand
    // Parameters:  const std::vector<std::wstring>& cmdTokens
    // Returns:     -
    // Comments:    See Safir::Application::Backdoor
    //
    void Services::HandleCommand(const std::vector<std::wstring>& cmdTokens)
    {
        for(unsigned int i=0; i < cmdTokens.size(); ++i)
        {
            if (cmdTokens[i] == L"overflow_true")
            {
                m_backdoorOverflow = true;
            }
            if (cmdTokens[i] == L"overflow_false")
            {
                m_backdoorOverflow = false;
            }

        }
        m_debug << "Force overflow is: " << m_backdoorOverflow << std::endl;
        //                m_connection.SetAlwaysOverflowFlag(m_backdoorOverflow);
    }

    // Function:    GetHelpText
    // Parameters:  -
    // Returns:     std::wstring, complete help text string
    // Comments:    See Safir::Application::Backdoor
    //
    std::wstring Services::GetHelpText()
    {
        std::wostringstream out;
        out << "backdoor commands:" << std::endl
            << "overflow true   Force overflow" << std::endl
            << "overflow false  Normal overflow routines" << std::endl;

        return out.str();
    }
}
}
};
