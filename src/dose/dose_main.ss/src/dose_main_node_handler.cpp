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

#include "dose_main_node_handler.h"

#include "dose_main_communication.h"
#include "dose_main_request_handler.h"
#include "dose_main_pool_handler.h"
#include "dose_main_defs.h"
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/DistributionChannelProperty.h>
#include <Safir/Dob/Internal/Connection.h>
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
#include <Safir/Dob/Internal/DoseCom_Interface.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    void CheckParameters()
    {
        if (Safir::Dob::NodeParameters::NodesArraySize() != Safir::Dob::NodeParameters::NumberOfNodes())
        {
            lllerr << "The parameter Safir.Dob.NodeParameters.NumberOfNodes" << std::endl
                   << "does not correspond to the size of the parameter array" << std::endl
                   << "Safir.Dob.NodeParameters.Nodes." << std::endl
                   << "Please correct this and try again!" << std::endl;
            exit(-1);
        }
        std::set<std::wstring> names;
        for (int i = 0; i < Safir::Dob::NodeParameters::NumberOfNodes(); ++i)
        {
            const bool inserted = names.insert(Safir::Dob::NodeParameters::Nodes(i)->NodeName()).second;
            if (!inserted)
            {
                lllerr << "The node names in Safir.Dob.NodeParameters.Nodes must be unique!" << std::endl
                       << "It appears that there is more than one '"
                       << Safir::Dob::NodeParameters::Nodes(i)->NodeName().GetVal() << "'" << std::endl
                   << "Please correct this and try again!" << std::endl;
                exit(-1);
            }
        }
    }

    NodeHandler::NodeHandler()
        : m_nodeStatuses(NUM_NODES,Dob::NodeStatus::Expected),
          m_ecom(NULL),
          m_requestHandler(NULL),
          m_poolHandler(NULL)
    {
    }

    NodeHandler::~NodeHandler()
    {
    }

    void NodeHandler::Init(ExternNodeCommunication & ecom,
                           RequestHandler & requestHandler,
                           PoolHandler& poolHandler)
    {
        CheckParameters();
        m_ecom = &ecom;
        m_requestHandler = &requestHandler;
        m_poolHandler = &poolHandler;

        if (!ecom.IsLocal(Dob::NodeInfo::ClassTypeId))
        {
            throw Dob::Typesystem::ConfigurationErrorException
                (L"Entity NodeInfo must have DistributionChannelProperty (or Override) set to Local",__WFILE__,__LINE__);
        }

        m_connection.Attach();
        m_connection.RegisterEntityHandler(Dob::NodeInfo::ClassTypeId,
                                           Typesystem::HandlerId(),
                                           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);

        // Set initial status for all nodes in the configuration
        for (int id = 0; id < Dob::NodeParameters::NumberOfNodes(); ++id)
        {
            Dob::NodeInfoPtr ni = Dob::NodeInfo::Create();

            ni->NodeName().SetVal(NodeParameters::Nodes(id)->NodeName().GetVal());

            if (id == ThisNodeParameters::NodeNumber())
            {
                ni->Status().SetVal(Dob::NodeStatus::Started);
                ni->IpAddress().SetVal(m_ecom->GetOwnIpAddress());
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
    }




    void NodeHandler::HandleNodeStatusChanges()
    {
        dcom_ulong32 id;        //out range 0-63
        dcom_ulong32 ns = 0;        //out = NODESTATUS_UP/DOWN
        dcom_ulong32 addr;

        Dob::NodeInfoPtr ni = Dob::NodeInfo::Create();

        while (DoseCom_GetNodeChange(id, ns, addr))
        {
            if (ns == 0) //spurious node status from dose_com
            {
                lllerr << "Got a spurious node status from dose_com for node " << ni << ", ignoring." <<std::endl;
                continue;
            }

            ni->NodeName() = NodeParameters::Nodes(id)->NodeName().GetVal();
            ni->IpAddress() = m_ecom->IpAddressToString(addr);

            switch (ns)
            {
            case NODESTATUS_NEW:
                {
                    lllerr << "NodeNew: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;

                    ni->Status() = Dob::NodeStatus::Starting;
                    m_nodeStatuses[id] = Dob::NodeStatus::Starting;
                }
                break;
            case NODESTATUS_UP:
                {
                    lllerr << "NodeUp: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;

                    ni->Status() = Dob::NodeStatus::Started;
                    m_nodeStatuses[id] = Dob::NodeStatus::Started;
                }
                break;
            case NODESTATUS_DOWN:
                {
                    lllerr << "NodeDown: " << ni->NodeName().GetVal() << " (id = " << id << ")"<< std::endl;

                    ni->Status() = Dob::NodeStatus::Failed;
                    m_nodeStatuses[id] = Dob::NodeStatus::Failed;

                    DeleteConnections(id);
                    m_poolHandler->RemoveStatesWaitingForNode(static_cast<Typesystem::Int32>(id));
                }
                break;
            default:
                ENSURE(false, << "Unexpected NodeStatus for node with id " << id << ", status = " << ns << "(" << (char)ns << ")");
            }

            m_connection.SetAll(ni,Typesystem::InstanceId(id),Typesystem::HandlerId());
        }
    }

    void NodeHandler::HandleDisconnect(const ConnectionPtr & connection, const NodeNumber node)
    {
        if (!connection->IsLocal() && m_nodeStatuses[node] == Dob::NodeStatus::Failed)
        {
            connection->SetNodeDown();
        }

        m_requestHandler->HandleDisconnect(connection);
    }

    void NodeHandler::DeleteConnections(const NodeNumber node)
    {
        Connections::Instance().RemoveConnectionFromNode(node, boost::bind(&NodeHandler::HandleDisconnect,this,_1,node));
    }



    void NodeHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                            const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        lllerr << "Someone overregistered Safir::Dob::NodeStatus, so I'm not "
            << "going to be able to update this entity any longer!" << std::endl;
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
