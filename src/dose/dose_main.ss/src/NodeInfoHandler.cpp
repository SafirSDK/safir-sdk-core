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
#include "NodeInfoHandler.h"

#if 0 //stewart
#include <Safir/Dob/AccessDeniedException.h>
#include <Safir/Dob/DistributionChannelProperty.h>
#include <Safir/Dob/Internal/Connection.h>
#include <Safir/Dob/Internal/EntityTypes.h>
#include <Safir/Dob/Internal/Connections.h>
#include <Safir/Dob/NodeInfo.h>
#include <Safir/Dob/NodeParameters.h>
#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/bind.hpp>
#endif

#include <Safir/Dob/ErrorResponse.h>
#include <Safir/Dob/ResponseGeneralErrorCodes.h>
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
    NodeInfoHandler::NodeInfoHandler()
    {
#if 0 //stewart
        m_connection.Attach();
        m_connection.RegisterEntityHandler(Dob::NodeInfo::ClassTypeId,
                                           Typesystem::HandlerId(),
                                           Safir::Dob::InstanceIdPolicy::HandlerDecidesInstanceId,
                                           this);

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






    void NodeInfoHandler::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId    /*typeId*/,
                                            const Safir::Dob::Typesystem::HandlerId& /*handlerId*/)
    {
        SEND_SYSTEM_LOG(Error,
                        << "Someone overregistered Safir::Dob::NodeStatus, so I'm not "
                        << "going to be able to update this entity any longer!");
    }


    void NodeInfoHandler::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                      Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send create requests on Safir::Dob::NodeStatus"));
    }

    void NodeInfoHandler::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                      Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send update requests on Safir::Dob::NodeStatus"));
    }

    void NodeInfoHandler::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                                      Safir::Dob::ResponseSenderPtr    responseSender)
    {
        responseSender->Send(Safir::Dob::ErrorResponse::CreateErrorResponse
            (Safir::Dob::ResponseGeneralErrorCodes::SafirReqErr(),
             L"It is not possible to send delete requests on Safir::Dob::NodeStatus"));
    }
}
}
}
