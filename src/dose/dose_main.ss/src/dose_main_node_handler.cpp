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

#include "dose_main_node_handler.h"

#include "dose_main_communication.h"
#include "dose_main_request_handler.h"
#include "PoolHandler.h"
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/DistributionChannelProperty.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/bind.hpp>

#if 0 //stewart
#include <Safir/Dob/Internal/DoseCom_Interface.h>
#endif

#include <Safir/Utilities/Internal/SystemLog.h>


namespace Safir
{
namespace Dob
{
namespace Internal
{
#if 0 //stewart
    void CheckParameters()
    {
        if (Safir::Dob::NodeParameters::NodesArraySize() != Safir::Dob::NodeParameters::NumberOfNodes())
        {
            throw Dob::Typesystem::ConfigurationErrorException(L"The parameter Safir.Dob.NodeParameters.NumberOfNodes\n"
                                                               L"does not correspond to the size of the parameter array\n"
                                                               L"Safir.Dob.NodeParameters.Nodes.\n"
                                                               L"Please correct this and try again!", __WFILE__, __LINE__);
        }
        std::set<std::wstring> names;
        for (int i = 0; i < Safir::Dob::NodeParameters::NumberOfNodes(); ++i)
        {
            const bool inserted = names.insert(Safir::Dob::NodeParameters::Nodes(i)->NodeName()).second;
            if (!inserted)
            {
                std::wostringstream ostr;
                ostr << "The node names in Safir.Dob.NodeParameters.Nodes must be unique!" << std::endl
                     << "It appears that there is more than one '"
                     << Safir::Dob::NodeParameters::Nodes(i)->NodeName().GetVal() << "'" << std::endl
                     << "Please correct this and try again!" << std::endl;
                throw Dob::Typesystem::ConfigurationErrorException(ostr.str(),__WFILE__, __LINE__);
            }
        }
    }
#endif
    NodeHandler::NodeHandler()
        :
#if 0 //stewart
        m_ecom(NULL),
#endif
          m_requestHandler(NULL),
          m_poolHandler(NULL)
    {
    }

    NodeHandler::~NodeHandler()
    {
    }

    void NodeHandler::Init(
#if 0 //stewart
                           ExternNodeCommunication & ecom,
#endif
                           RequestHandler & requestHandler,
                           PoolHandler& poolHandler)
    {
#if 0 //stewart
        CheckParameters();
        m_ecom = &ecom;
#endif

        m_requestHandler = &requestHandler;
        m_poolHandler = &poolHandler;

#if 0 //stewart
        if (!ecom.IsLocal(Dob::NodeInfo::ClassTypeId))
        {
            throw Dob::Typesystem::ConfigurationErrorException
                (L"Entity NodeInfo must have DistributionChannelProperty (or Override) set to Local",__WFILE__,__LINE__);
        }
#endif

        m_connection.Attach();
        m_connection.RegisterEntityHandler(Dob::NodeInfo::ClassTypeId,
                                           Typesystem::HandlerId(),
                                           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);
#if 0 //stewart

        //TODO stewart: maybe make this a class constant
        const int64_t thisNodeId = Connections::Instance().NodeId();

        // Set initial status for all nodes in the configuration
        for (int id = 0; id < Dob::NodeParameters::NumberOfNodes(); ++id)
        {
            Dob::NodeInfoPtr ni = Dob::NodeInfo::Create();

            ni->NodeName().SetVal(NodeParameters::Nodes(id)->NodeName().GetVal());

            if (id == thisNodeId)
            {
                ni->Status().SetVal(Dob::NodeStatus::Started);
                ni->IpAddress().SetVal(m_ecom->GetOwnIpAddress());
                //stewart todo
                ni->IpAddress().SetVal(L"127.0.0.1");

            }
            else
            {
                ni->Status().SetVal(Dob::NodeStatus::Expected);
            }

            try
            {
                m_connection.SetAll(ni,Typesystem::InstanceId(id),Typesystem::HandlerId());
            }
            catch (const Safir::Dob::AccessDeniedException &)
            {
                //I've been overregistered, nothing to do...
            }
        }


        if (!m_ecom->GetQualityOfServiceData().IsStandalone())
        {
            HandleNodeStatusChanges();
        }
#endif
    }




    void NodeHandler::HandleNodeStatusChanges()
    {
#if 0 //stewart
        dcom_ulong32 id;        //out range 0-63
        dcom_ulong32 ns = 0;        //out = NODESTATUS_UP/DOWN
        dcom_ulong32 addr;

        Dob::NodeInfoPtr ni = Dob::NodeInfo::Create();

        while (DoseCom_GetNodeChange(id, ns, addr))
        {
            if (ns == 0) //spurious node status from dose_com
            {
                SEND_SYSTEM_LOG(Error,
                                << "Got a spurious node status from dose_com for node " << ni << ", ignoring.");
                continue;
            }

            ni->NodeName() = NodeParameters::Nodes(id)->NodeName().GetVal();
            ni->IpAddress() = m_ecom->IpAddressToString(addr);

            switch (ns)
            {
            case NODESTATUS_NEW:
                {
                    //TODO If we're going to log these in stewart we need to use LogStatus fcn
                    lllog(0) << "NodeNew: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;
                    std::wcout << "NodeNew: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;

                    ni->Status() = Dob::NodeStatus::Starting;
                    NodeStatuses::Instance().SetNodeStatus(id, Dob::NodeStatus::Starting);
                }
                break;
            case NODESTATUS_UP:
                {
                    //TODO If we're going to log these in stewart we need to use LogStatus fcn
                    lllog(0) << "NodeUp: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;
                    std::wcout << "NodeUp: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;

                    ni->Status() = Dob::NodeStatus::Started;
                    NodeStatuses::Instance().SetNodeStatus(id, Dob::NodeStatus::Started);
                }
                break;
            case NODESTATUS_DOWN:
                {
                    //TODO If we're going to log these in stewart we need to use LogStatus fcn
                    lllog(0) << "NodeDown: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;
                    std::wcout << "NodeDown: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;

                    ni->Status() = Dob::NodeStatus::Failed;
                    NodeStatuses::Instance().SetNodeStatus(id, Dob::NodeStatus::Failed);

                    DeleteConnections(id);
                    m_poolHandler->RemoveStatesWaitingForNode(static_cast<Typesystem::Int32>(id));
                }
                break;
            default:
                ENSURE(false, << "Unexpected NodeStatus for node with id " << id << ", status = " << ns << "(" << (char)ns << ")");
            }

            m_connection.SetAll(ni,Typesystem::InstanceId(id),Typesystem::HandlerId());
        }


        if (!NodeStatuses::Instance().AnyNodeHasStatus(NodeStatus::Starting) &&
            !NodeStatuses::Instance().AnyNodeHasStatus(NodeStatus::Failed))
        {
            // Ok, it seems that all nodes are either Started or Expected (has never been started).
            // In this case we know that we have got the pools, and thus the ghosts, from all nodes so
            // we can clean-up the ghosts and only save the ones from the newest registration.

            EntityTypes::Instance().CleanGhosts();

            // Kick all connections so they will do an dispatch.
            Connections::Instance().ForEachConnectionPtr(boost::bind(&NodeHandler::KickConnection,this,_1));

        }
#endif
    }

    void NodeHandler::HandleDisconnect(const ConnectionPtr& connection, const int64_t node)
    {
#if 0 //stewart
        if (!connection->IsLocal() && NodeStatuses::Instance().GetNodeStatus(node) == Dob::NodeStatus::Failed)
        {
            connection->SetNodeDown();
        }
#endif
        m_requestHandler->HandleDisconnect(connection);
    }

    void NodeHandler::DeleteConnections(const int64_t node)
    {
        Connections::Instance().RemoveConnectionFromNode(node, boost::bind(&NodeHandler::HandleDisconnect,this,_1,node));
    }

    void NodeHandler::KickConnection(const ConnectionPtr& connection)
    {
        connection->SignalIn();
    }

    void NodeHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                            const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Error,
                        << "Someone overregistered Safir::Dob::NodeStatus, so I'm not "
                        << "going to be able to update this entity any longer!");
    }


    void NodeHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                      Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send create requests on Safir::Dob::NodeStatus"));
    }

    void NodeHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                      Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send update requests on Safir::Dob::NodeStatus"));
    }

    void NodeHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                      Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send delete requests on Safir::Dob::NodeStatus"));
    }
}
}
}
