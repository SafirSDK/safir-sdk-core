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

#include "dose_main_quality_of_service_data.h"

#include <Safir/Dob/ThisNodeParameters.h>
#include <Safir/Dob/DistributionChannelAliases.h>
#include <Safir/Dob/DistributionChannelOverrideProperty.h>
#include <Safir/Dob/DistributionChannelProperty.h>
#include <Safir/Dob/DistributionChannel.h>
#include <Safir/Dob/DistributionChannelParameters.h>
#include <Safir/Dob/Priority.h>
#include <Safir/Dob/PriorityParameters.h>
#include <Safir/Dob/PriorityAliasParameters.h>
#include <Safir/Dob/PriorityOverrideProperty.h>
#include <Safir/Dob/PriorityProperty.h>
#include <algorithm>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Exceptions.h>
#include <iostream>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <string>
#include "dose_main_defs.h"
#include <bitset>
#include <Safir/Dob/Typesystem/Internal/InternalUtils.h>
#include <Safir/Dob/Entity.h>
#include <Safir/Dob/Service.h>
#include <Safir/Dob/Message.h>
#include <Safir/Dob/Response.h>
#include <boost/regex.hpp>

static const std::wstring LLL_PREFIX = L"Dose::QoS ";

namespace Safir
{
namespace Dob
{
namespace Internal
{


    QualityOfServiceData::QualityOfServiceData():
        m_bIsStandalone(false),
        m_AckedPriorities(NUM_PRIORITY_CHANNELS,true)
    {

    }

    QualityOfServiceData::~QualityOfServiceData()
    {

    }

    int QualityOfServiceData::PriorityFromName(const std::wstring & name)
    {
        if (name.empty())
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal priority name",__WFILE__,__LINE__);
        }

        NameIdTable::iterator thePrio = m_PriorityNameTable.find(name);
        if (thePrio == m_PriorityNameTable.end())
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"prio not found",__WFILE__,__LINE__);
        }
        else
        {
            return thePrio->second;
        }
    }

    int QualityOfServiceData::DistributionChannelFromName(const std::wstring & name)
    {
        if (name.empty())
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal channel name",__WFILE__,__LINE__);
        }

        NameIdTable::iterator theChannel = m_DistributionChannelNameTable.find(name);
        if (theChannel == m_DistributionChannelNameTable.end())
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"channel not found",__WFILE__,__LINE__);
        }
        else if (theChannel->second == POOL_DISTRIBUTION_CHANNEL)
        {
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"It is illegal for types to be distributed on the PoolDistribution distribution channel.",__WFILE__,__LINE__);
        }
        else
        {
            return theChannel->second;
        }
    }

    int QualityOfServiceData::GetPriority(const Dob::Typesystem::TypeId typeId)
    {
        std::wstring prioName;
        bool isInherited,hasProperty;

        Dob::Typesystem::Operations::HasProperty(typeId,Dob::PriorityOverrideProperty::ClassTypeId, hasProperty, isInherited);

        bool foundPrio = false;
        if (hasProperty && !isInherited) //Make sure we do not get an inherited override!
        {
            try
            {
                //unfortunately we have to create dummy object here to be able to read the property, even
                //though it is a constant
                Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                prioName = Dob::PriorityOverrideProperty::GetPriority(obj);
            }
            catch (...)
            {
                std::wcout << "Property member 'Priority' of property "
                           << Dob::Typesystem::Operations::GetName(Dob::PriorityOverrideProperty::ClassTypeId)
                           << " for class "
                           << Dob::Typesystem::Operations::GetName(typeId)
                           << " is not mapped to a parameter!" <<std::endl;
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
            }
            lllout << LLL_PREFIX << "Class " << Dob::Typesystem::Operations::GetName(typeId) << " has OVERRIDDEN Priority property (" << prioName << ")" << std::endl;
            foundPrio = true;
        }
        else
        {
            Dob::Typesystem::Operations::HasProperty(typeId,Dob::PriorityProperty::ClassTypeId, hasProperty, isInherited);

            if (hasProperty)
            {
                try
                {
                    Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                    prioName = Dob::PriorityProperty::GetPriority(obj);
                }
                catch (...)
                {
                    std::wcout << "Property member 'Priority' of property "
                        << Dob::Typesystem::Operations::GetName(Dob::PriorityProperty::ClassTypeId)
                        << " for class "
                        << Dob::Typesystem::Operations::GetName(typeId)
                        << " is not mapped to a parameter!" <<std::endl;
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
                }
                lllout << LLL_PREFIX << "Class " << Dob::Typesystem::Operations::GetName(typeId) << " has "
                    << (isInherited?"INHERITED":"OWN")
                    << " Priority property ("<< prioName << ")" << std::endl;
                foundPrio = true;
            }
        }

        if(!foundPrio)
        {
            std::wcout << "WARNING: Class " << Dob::Typesystem::Operations::GetName(typeId)
                       << " does not have a Priority defined!" <<std::endl;
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);

        }

        return PriorityFromName(prioName);
    }


    int QualityOfServiceData::GetDistributionChannel(const Dob::Typesystem::TypeId typeId)
    {
        bool isInherited, hasProperty;
        std::wstring channelName;

        Dob::Typesystem::Operations::HasProperty(typeId,Dob::DistributionChannelOverrideProperty::ClassTypeId, hasProperty, isInherited);

        bool foundDc = false;
        if (hasProperty && !isInherited) //Make sure we do not get an inherited override!
        {
            try
            {
                Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                channelName = Dob::DistributionChannelOverrideProperty::GetDistributionChannel(obj);
            }
            catch (...)
            {
                std::wcout << "Property member 'DistributionChannel' of property "
                           << Dob::Typesystem::Operations::GetName(Dob::DistributionChannelOverrideProperty::ClassTypeId)
                           << " for class "
                           << Dob::Typesystem::Operations::GetName(typeId)
                           << " is not mapped to a parameter!" <<std::endl;
                throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
            }
            lllout << LLL_PREFIX << "Class " << Dob::Typesystem::Operations::GetName(typeId) << " has OVERRIDDEN DistributionChannel property (" << channelName << ")" << std::endl;
            foundDc = true;
        }
        else
        {
            Dob::Typesystem::Operations::HasProperty(typeId,Dob::DistributionChannelProperty::ClassTypeId, hasProperty, isInherited);

            if (hasProperty)
            {
                try
                {
                    Dob::Typesystem::ObjectPtr obj = Typesystem::ObjectFactory::Instance().CreateObject(typeId);
                    channelName = Dob::DistributionChannelProperty::GetDistributionChannel(obj);
                }
                catch (...)
                {
                    std::wcout << "Property member 'DistributionChannel' of property "
                               << Dob::Typesystem::Operations::GetName(Dob::DistributionChannelProperty::ClassTypeId)
                               << " for class "
                               << Dob::Typesystem::Operations::GetName(typeId)
                               << " is not mapped to a parameter!" <<std::endl;
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
                }
                lllout << LLL_PREFIX << "Class " << Dob::Typesystem::Operations::GetName(typeId) << " has "
                           << (isInherited?"INHERITED":"OWN")
                           << " DistributionChannel property (" << channelName << ")" << std::endl;
                foundDc = true;
            }
        }

        if (!foundDc)
        {
            std::wcout << "Class " << Dob::Typesystem::Operations::GetName(typeId)
                       << " does not have a DistributionChannel defined!" <<std::endl;
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
        }
        return DistributionChannelFromName(channelName);
    }

    void
    QualityOfServiceData::ReadPriorities()
    {
        bool foundOneAckedPrio = false;
        try
        {
            for (int ix = 0; ix < Dob::PriorityParameters::PrioritiesArraySize(); ++ix)
            {
                Dob::PriorityPtr prioItem = Dob::PriorityParameters::Priorities(ix);
                const int prio = prioItem->PriorityLevel().GetVal();
                if (prio < 0 || prio >= NUM_PRIORITY_CHANNELS)
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal priority",__WFILE__,__LINE__);
                }

                const std::wstring prioName = prioItem->Name().GetVal();
                if (prioName.empty())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"illegal priority name",__WFILE__,__LINE__);
                }

                m_PriorityNameTable.insert(std::make_pair(prioName,prio));
                m_AckedPriorities[prio] = prioItem->Acknowledged().GetVal();
                lllout << LLL_PREFIX << "Read priority "
                           << prio
                           << " with name '"
                           << prioName
                           << "': "
                           << (!m_AckedPriorities[prio]?"NOT ":"")
                           << "acknowledged" << std::endl;
                if (m_AckedPriorities[prio])
                {
                    foundOneAckedPrio = true;
                }
            }
        }
        catch (const Dob::Typesystem::FundamentalException &)
        {
            std::wcout << "Failed to read QoS priorities parameters" <<std::endl;
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
        }

        if (!foundOneAckedPrio)
        {
           std::wcout << "There needs to be at least one priority that is acknowledged!" <<std::endl;
           throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
        }
    }

    void
    QualityOfServiceData::ReadPriorityAliases()
    {
        NameIdTable aliases;
        try
        {
            for (int ix = 0; ix < Dob::PriorityAliasParameters::AliasesArraySize(); ++ix)
            {
                Dob::AliasPtr aliasItem = Dob::PriorityAliasParameters::Aliases(ix);
                const std::wstring alias = aliasItem->AliasName().GetVal();
                const std::wstring prioName = aliasItem->RealName().GetVal();

                //check that the alias name doesnt conflict with a priority
                if (alias.empty() || m_PriorityNameTable.find(alias) != m_PriorityNameTable.end())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal alias name",__WFILE__,__LINE__);
                }

                if (prioName.empty())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal priority name in alias",__WFILE__,__LINE__);
                }

                //find the priority that the alias points to
                NameIdTable::iterator thePrio = m_PriorityNameTable.find(prioName);

                //did it exist?
                if (thePrio == m_PriorityNameTable.end())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal priority name in alias",__WFILE__,__LINE__);
                }
                aliases.insert(std::make_pair(alias,thePrio->second));
            }
        }
        catch (const Dob::Typesystem::FundamentalException &)
        {
            std::wcout << "Failed to read QoS priorities parameters" <<std::endl;
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
        }

        //merge the aliases with the priorities
        m_PriorityNameTable.insert(aliases.begin(),aliases.end());
    }

    //theSingleNode is only valid if numNodes is 1.
    void
    ConvertNodesString(const std::wstring & includedNodes, boost::uint64_t & converted, int & numNodes, int & theSingleNode)
    {
        if (includedNodes.size() != static_cast<size_t>(NUM_NODES))
        {
            throw Dob::Typesystem::ConfigurationErrorException
                               (L"Length of IncludedNodes string must be 64",__WFILE__,__LINE__);
        }

        converted = 0;
        numNodes = 0;

        for (std::wstring::const_reverse_iterator it = includedNodes.rbegin(); it!= includedNodes.rend();++it)
        {
            if (*it == '-')
            {
                converted = converted << 1; //add a 0 to the end
            }
            else if (*it == '+')
            {
                converted = converted << 1 | 0x1; //add a 1 to the end
                theSingleNode = static_cast<int>(std::distance(it,includedNodes.rend()) - 1);
                ++numNodes;
            }
            else
            {
                throw Dob::Typesystem::ConfigurationErrorException(L"Illegal character in IncludedNodes string (can only be + or -)",__WFILE__,__LINE__);
            }
        }
    }

    bool CheckAddress(const std::string & addr)
    {
        static const boost::regex expr("(\\d{1,3}\\.){3}\\d{1,3}");
        return boost::regex_match(addr,expr);
    }

    void
    QualityOfServiceData::ReadDistributionChannels()
    {
        //insert Local channel first
        m_DistributionChannelNameTable.insert(std::make_pair(L"Local",LOCAL_DISTRIBUTION_CHANNEL));

        int nextChannelNumber = SYSTEM_DISTRIBUTION_CHANNEL; //start at 64. System must always get this number
        try
        {
            for (int ix = 0; ix < Dob::DistributionChannelParameters::DistributionChannelsArraySize(); ++ix)
            {
                Dob::DistributionChannelPtr channelItem = Dob::DistributionChannelParameters::DistributionChannels(ix);

                const std::wstring name = channelItem->Name().GetVal();
                if (name.empty())
                {
                    throw Dob::Typesystem::ConfigurationErrorException(L"Illegal distribution channel name",__WFILE__,__LINE__);
                }

                boost::uint64_t includedNodes;
                int numNodesInChannel;
                int theSingleNode; //is only used when numNodesInChannel == 1
                ConvertNodesString(channelItem->IncludedNodes().GetVal(),includedNodes,numNodesInChannel, theSingleNode);

                lllout << LLL_PREFIX << "Read DistributionChannel '" << name << "' ";

                //special handling for the System distribution channel.
                if (ix == 0)
                {
                    //check that the first one is System
                    if(name != L"System")
                    {
                        throw Dob::Typesystem::ConfigurationErrorException(L"System must be first defined distribution channel",__WFILE__,__LINE__);
                    }

                    const std::string multicastAddress = channelItem->MulticastAddress().Utf8String();
                    if (multicastAddress == "127.0.0.1")
                    {
                        m_bIsStandalone = true;
                        lllout << " which is local (all messages will be local and no other channels or priorities will be read)" <<std::endl;
                        return;
                    }
                }
                if (ix == 1)
                {
                    //check that the second one is PoolDistribution
                    if(name != L"PoolDistribution")
                    {
                        throw Dob::Typesystem::ConfigurationErrorException(L"PoolDistribution must be second defined distribution channel",__WFILE__,__LINE__);
                    }
                }

                if (numNodesInChannel == 0)
                {
                    throw Dob::Typesystem::ConfigurationErrorException(L"Channel must contain at least one node",__WFILE__,__LINE__);
                }
                else if (numNodesInChannel == 1)
                {
                    //is this node the single node --> use Local
                    const int nodeId=Dob::ThisNodeParameters::NodeNumber();
                    if (theSingleNode == nodeId)
                    {
                        m_DistributionChannelNameTable.insert(std::make_pair(name,LOCAL_DISTRIBUTION_CHANNEL));
                        lllout <<  "which is this node, so data will be local" << std::endl;
                    }
                    else
                    {
                        //use singlecast for this distribution channel name
                        m_DistributionChannelNameTable.insert(std::make_pair(name,theSingleNode));
                        lllout << "which will use singlecast to node " << theSingleNode <<std::endl;
                    }
                }
                else
                {
                    const std::string multicastAddress = channelItem->MulticastAddress().Utf8String();
                    if (!CheckAddress(multicastAddress))
                    {
                        throw Dob::Typesystem::ConfigurationErrorException(std::wstring(L"Illegal multicast address: ") +
                                                                           channelItem->MulticastAddress().GetVal(),__WFILE__,__LINE__);
                    }

                    m_DistributionChannelTable.insert(std::make_pair
                                                      (nextChannelNumber,DistributionChannelData(includedNodes,multicastAddress)));
                    m_DistributionChannelNameTable.insert(std::make_pair(name,nextChannelNumber));

                    ++nextChannelNumber;

                    lllout << " (id="
                             << nextChannelNumber - 1
                             << ") with included nodes '"
                             << std::hex << includedNodes << std::dec
                             << "'" <<std::endl;
                }
            }

        }
        catch (const Dob::Typesystem::FundamentalException &)
        {
            std::wcout << "Failed to read QoS DistributionChannel parameters" <<std::endl;
            throw;
        }
    }

    void
    QualityOfServiceData::ReadDistributionChannelAliases()
    {
        if (IsStandalone())
        {
            return;
        }

        NameIdTable aliases;
        try
        {
            for (int ix = 0; ix < Dob::DistributionChannelAliases::AliasesArraySize(); ++ix)
            {
                Dob::AliasPtr aliasItem = Dob::DistributionChannelAliases::Aliases(ix);
                const std::wstring alias = aliasItem->AliasName().GetVal();
                const std::wstring channelName = aliasItem->RealName().GetVal();

                //check that the alias name doesnt conflict with a real channel
                if (alias.empty() || m_DistributionChannelNameTable.find(alias) != m_DistributionChannelNameTable.end())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal alias name",__WFILE__,__LINE__);
                }

                if (channelName.empty())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal distribution channel name in alias",__WFILE__,__LINE__);
                }

                //find the channel that the alias points to
                NameIdTable::iterator theChannel = m_DistributionChannelNameTable.find(channelName);

                //did it exist?
                if (theChannel == m_DistributionChannelNameTable.end())
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"Illegal channel name in alias",__WFILE__,__LINE__);
                }
                if (theChannel->second == POOL_DISTRIBUTION_CHANNEL)
                {
                    throw Safir::Dob::Typesystem::SoftwareViolationException(L"No alias may point to PoolDistribution channel!",__WFILE__,__LINE__);
                }
                aliases.insert(std::make_pair(alias,theChannel->second));
            }
        }
        catch (const Dob::Typesystem::FundamentalException &)
        {
            std::wcout << "Failed to read QoS channel parameters" <<std::endl;
            throw Safir::Dob::Typesystem::SoftwareViolationException(L"QoS failure",__WFILE__,__LINE__);
        }

        //merge the aliases with the channel
        m_DistributionChannelNameTable.insert(aliases.begin(),aliases.end());
    }



    void
    QualityOfServiceData::ReadParameters()
    {
        ReadDistributionChannels();
        if (!IsStandalone())
        {
            ReadDistributionChannelAliases();
            ReadPriorities();
            ReadPriorityAliases();
        }

    }

    void
    QualityOfServiceData::Init()
    {
        ReadParameters();

        if (!IsStandalone())
        {
            Dob::Typesystem::TypeIdVector typeIds = Safir::Dob::Typesystem::Operations::GetAllTypeIds();

            for (Dob::Typesystem::TypeIdVector::iterator it = typeIds.begin();
                 it != typeIds.end();
                 ++it)
            {
                if (Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Entity::ClassTypeId) ||
                    Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Service::ClassTypeId) ||
                    Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Message::ClassTypeId) ||
                    Dob::Typesystem::Operations::IsOfType(*it,Safir::Dob::Response::ClassTypeId))
                {
                    m_QoSTable.insert(QoSTable::value_type(*it,QoSData(GetDistributionChannel(*it),
                        GetPriority(*it))));
                }
            }
        }
    }



    void
    QualityOfServiceData::GetQualityOfServiceInfo(const Dob::Typesystem::TypeId typeId, //in
                                                  int & distributionChannel,        //out
                                                  int & priority,                   //out
                                                  bool & isAcked) const             //out
    {
        QoSTable::const_iterator result = m_QoSTable.find(typeId);
        if (result == m_QoSTable.end())
        {
            std::wostringstream ostr;
            ostr << "QualityOfServiceData::GetQualityOfServiceInfo: Type "
                << typeId << " ("
                << Typesystem::Operations::GetName(typeId) << ")"
                << " was not found in the QoS tables.";
            throw Dob::Typesystem::SoftwareViolationException
                                (ostr.str(),__WFILE__,__LINE__);
        }

        distributionChannel = result->second.distributionChannel;
        priority = result->second.priority;

        isAcked = m_AckedPriorities[priority];
    }

    void
    QualityOfServiceData::GetQualityOfServiceInfoForInternalDistribution(int & distributionChannel,        //out
                                                                         int & priority,                   //out
                                                                         bool & isAcked) const
    {
        distributionChannel = SYSTEM_DISTRIBUTION_CHANNEL;
        isAcked = true;
        //find the lowest prio which is acked
        AckedPriorities::const_reverse_iterator thePrio = std::find(m_AckedPriorities.rbegin(),m_AckedPriorities.rend(),true);
        priority = static_cast<int>(std::distance(thePrio,m_AckedPriorities.rend())) - 1;
    }

    void
    QualityOfServiceData::GetQualityOfServiceInfoForPoolDistribution(int & distributionChannel,        //out
                                                                     int & priority,                   //out
                                                                     bool & isAcked) const
    {
        distributionChannel = POOL_DISTRIBUTION_CHANNEL;
        isAcked = true;
        //find the lowest prio which is acked
        AckedPriorities::const_reverse_iterator thePrio = std::find(m_AckedPriorities.rbegin(),m_AckedPriorities.rend(),true);
        priority = static_cast<int>(std::distance(thePrio,m_AckedPriorities.rend())) - 1;
    }

    int
    QualityOfServiceData::GetClosestPriority(const int     priority,
                                             const bool    acked) const
    {
        for (int i = priority - 1; i >= 0; --i)
        {
            if (acked == m_AckedPriorities[i])
            {
                return i;
            }
        }

        for (int i = priority + 1; i < NUM_PRIORITY_CHANNELS; ++i)
        {
            if (acked == m_AckedPriorities[i])
            {
                return i;
            }
        }
        ENSURE(false, << "Failed to GetClosestPriority, probably a programming error by STLRHA or AIWI");
        return -1;
    }

    bool
    QualityOfServiceData::IsNodeInDistributionChannel(const Dob::Typesystem::TypeId typeId, const NodeNumber node) const
    {
        if (IsStandalone())
        {
            return false;
        }

        QoSTable::const_iterator result = m_QoSTable.find(typeId);
        if (result == m_QoSTable.end())
        {
            throw Dob::Typesystem::SoftwareViolationException
                                (L"TypeId was not found in the QoS tables.",__WFILE__,__LINE__);
        }

        if (result->second.distributionChannel == LOCAL_DISTRIBUTION_CHANNEL)
        {
            return false;
        }

        boost::uint64_t mask = 1;
        mask <<= node;
        return (GetDistributionChannelTable().find(result->second.distributionChannel)->second.includedNodes & mask) != 0;
    }


}
}
}

