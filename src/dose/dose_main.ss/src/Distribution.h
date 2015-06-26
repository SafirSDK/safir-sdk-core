/******************************************************************************
*
* Copyright Consoden AB, 2015 (http://safir.sourceforge.net)
*
* Created by: Anders Widn / anders.widen@consoden.se
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
#pragma once

#include <Safir/Dob/DistributionScopeOverrideProperty.h>
#include <Safir/Dob/DistributionScopeProperty.h>
#include <Safir/Dob/Internal/Communication.h>
#include <Safir/Dob/Internal/ControlConfig.h>
#include <Safir/Dob/Internal/SystemPicture.h>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <boost/chrono.hpp>
#include <boost/noncopyable.hpp>
#include <functional>
#include <map>
#include <string>
#include <vector>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4267)
#endif

#include <boost/asio.hpp>

#ifdef _MSC_VER

#pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
    typedef std::function<void(const std::string&  nodeName,
                               int64_t             nodeId,
                               int64_t             nodeTypeId,
                               const std::string&  dataAddress)> OnInjectNode;

    typedef std::function<void(int64_t nodeId,
                               int64_t nodeTypeId)> OnExcludeNode;

    // Class that encapsulates the Communication and System Picture instances
    //
    template <typename CommunicationT, typename SystemPictureT, typename ConfigT>
    class DistributionBasic
        : private boost::noncopyable
    {
    public:

        DistributionBasic(boost::asio::io_service&  ioService,
                          const std::string&        ownNodeName,
                          int64_t                   ownNodeId,
                          int64_t                   ownNodeTypeId,
                          const std::string&        ownDataAddress)
            : m_nodeId(ownNodeId),
              m_communication(),
              m_sp(),
              m_localTypes(CalculateLocalTypes()),
              m_nodeTypeIds(CalculateNodeTypeIds(m_config)),
              m_started(false)
        {
            // Create and populate structures that are needed when creating the Communication and
            // SP instances.
            std::vector<Com::NodeTypeDefinition> commNodeTypes;
            std::map<boost::int64_t, typename SP::NodeType> spNodeTypes;

            for (const auto& nt: m_config.nodeTypesParam)
            {
                commNodeTypes.push_back(Com::NodeTypeDefinition(nt.id,
                                         nt.name,
                                         nt.multicastAddressControl,
                                         nt.multicastAddressData,
                                         nt.heartbeatInterval,
                                         nt.retryTimeout,
                                         nt.maxLostHeartbeats));

                spNodeTypes.insert(std::make_pair(nt.id,
                                                  SP::NodeType(nt.id,
                                                               nt.name,
                                                               nt.isLight,
                                                               boost::chrono::milliseconds(nt.heartbeatInterval),
                                                               nt.maxLostHeartbeats,
                                                               boost::chrono::milliseconds(nt.retryTimeout))));
            }

            m_communication.reset(new CommunicationT(Com::dataModeTag,
                                                     ioService,
                                                     ownNodeName,
                                                     ownNodeId,
                                                     ownNodeTypeId,
                                                     ownDataAddress,
                                                     commNodeTypes));

            m_sp.reset(new SystemPictureT(SP::slave_tag,
                                          ioService,
                                          *m_communication,
                                          ownNodeName,
                                          ownNodeId,
                                          ownNodeTypeId,
                                          spNodeTypes));

        }

        void Start()
        {
            m_communication->Start();
            m_started = true;
        }

        // Stop the internal workings of this class.
        // Must be called before destroying the object.
        void Stop()
        {
            m_sp->Stop();
            m_communication->Stop();
            m_started = false;
        }

        //subscribe for new injected nodes and excluded nodes.
        void SubscribeNodeEvents(const OnInjectNode& onInjectNode, const OnExcludeNode& onExcludeNode)
        {
            ENSURE(!m_started, << "SubscribeNodeEvents must be called before Start!");

            m_injectCallbacks.push_back(onInjectNode);
            m_excludeCallbacks.push_back(onExcludeNode);
        }

        // Inject an external node
        void InjectNode(const std::string& nodeName,
                        int64_t            nodeId,
                        int64_t            nodeTypeId,
                        const std::string& dataAddress)
        {
            m_communication->InjectNode(nodeName,
                                        nodeId,
                                        nodeTypeId,
                                        dataAddress);

            for (auto& cb : m_injectCallbacks)
            {
                cb(nodeName, nodeId, nodeTypeId, dataAddress);
            }
        }

        void ExcludeNode(int64_t nodeId, int64_t nodeTypeId)
        {
            for (auto& cb : m_excludeCallbacks)

            {
                cb(nodeId, nodeTypeId);
            }
        }


        CommunicationT& GetCommunication()
        {
            return *m_communication;
        }

        const CommunicationT& GetCommunication() const
        {
            return *m_communication;
        }

        const ConfigT& GetNodeTypeConfiguration() const
        {
            return m_config;
        }

        int64_t GetNodeId() const
        {
            return m_nodeId;
        }

        const std::vector<int64_t>& GetNodeTypeIds() const
        {
            return m_nodeTypeIds;
        }

        //Check if type is local. If false is returned it is global...
        bool IsLocal(Safir::Dob::Typesystem::TypeId tid) const
        {
            return std::binary_search(m_localTypes.begin(), m_localTypes.end(), tid);
        }

    private:
        static bool ReadDistributionScopeProperty(const Safir::Dob::Typesystem::TypeId typeId)
        {
            bool isInherited,hasProperty;

            Dob::Typesystem::Operations::HasProperty(typeId,
                                                     Dob::DistributionScopeOverrideProperty::ClassTypeId,
                                                     hasProperty,
                                                     isInherited);

            if (hasProperty && !isInherited) //Make sure we do not get an inherited override!
            {
                try
                {
                    //unfortunately we have to create dummy object here to be able to read the property, even
                    //though it is a constant
                    Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                    return Dob::DistributionScopeOverrideProperty::GetDistributionScope(obj) ==
                        Dob::DistributionScope::Enumeration::Local;
                }
                catch (const std::exception & exc)
                {
                    std::wostringstream ostr;
                    ostr << "Failed to read Property member 'DistributionScope' of property "
                         << Dob::Typesystem::Operations::GetName(Dob::DistributionScopeOverrideProperty::ClassTypeId)
                         << " for class "
                         << Dob::Typesystem::Operations::GetName(typeId)
                         << ". Got exception " << exc.what();
                    throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
                }
            }
            else if (Dob::Typesystem::Operations::HasProperty(typeId,Dob::DistributionScopeProperty::ClassTypeId))
            {
                try
                {
                    Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                    return Dob::DistributionScopeProperty::GetDistributionScope(obj) ==
                        Dob::DistributionScope::Enumeration::Local;
                }
                catch (const std::exception & exc)
                {
                    std::wostringstream ostr;
                    ostr << "Failed to read Property member 'DistributionScope' of property "
                         << Dob::Typesystem::Operations::GetName(Dob::DistributionScopeProperty::ClassTypeId)
                         << " for class "
                         << Dob::Typesystem::Operations::GetName(typeId)
                         << ". Got exception " << exc.what();
                    throw Safir::Dob::Typesystem::SoftwareViolationException(ostr.str(),__WFILE__,__LINE__);
                }
            }

            return false;

        }

        static std::vector<Safir::Dob::Typesystem::TypeId> CalculateLocalTypes()
        {
            std::vector<Safir::Dob::Typesystem::TypeId> localTypes;

            lllog(3) << "Reading DistributionScope properties to ascertain local types" << std::endl;
            for (const auto typeId : Safir::Dob::Typesystem::Operations::GetAllTypeIds())
            {
                if (ReadDistributionScopeProperty(typeId))
                {
                    localTypes.push_back(typeId);
                    lllog(3) << "Local type " << Dob::Typesystem::Operations::GetName(typeId) << std::endl;
                }
            }

            std::sort(localTypes.begin(),localTypes.end());
            localTypes.shrink_to_fit();
            return std::move(localTypes);
        }

        static std::vector<int64_t> CalculateNodeTypeIds(const ConfigT& config)
        {
            std::vector<int64_t> nodeTypeIds;

            for (const auto& nt: config.nodeTypesParam)
            {
                nodeTypeIds.push_back(nt.id);
            }

            std::sort(nodeTypeIds.begin(),nodeTypeIds.end());
            nodeTypeIds.shrink_to_fit();
            return std::move(nodeTypeIds);
        }

        const int64_t m_nodeId;
        std::unique_ptr<CommunicationT> m_communication;
        std::unique_ptr<SystemPictureT> m_sp;
        const ConfigT m_config;
        std::vector<OnInjectNode> m_injectCallbacks;
        std::vector<OnExcludeNode> m_excludeCallbacks;

        //this is a sorted vector of typeids that are local
        //it is a vector rather than a set since it is likely to be small and
        //lookups are frequent.
        const std::vector<Safir::Dob::Typesystem::TypeId> m_localTypes;

        //this is a sorted vector of node type ids
        //it is a vector rather than a set since it is likely to be small and
        //we're going to loop over it frequently.
        const std::vector<int64_t> m_nodeTypeIds;

        bool m_started;
    };

    typedef DistributionBasic<Com::Communication, SP::SystemPicture, Control::Config> Distribution;
}
}
}
