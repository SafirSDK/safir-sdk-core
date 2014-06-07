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

#ifndef __DOSE_QUALITY_OF_SERVICE_DATA_H__
#define __DOSE_QUALITY_OF_SERVICE_DATA_H__

#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Internal/InternalDefs.h>
#include <Safir/Dob/Internal/ConnectionId.h>
#include <vector>
#include <map>
#include <boost/cstdint.hpp>
#include <boost/noncopyable.hpp>
#include <boost/unordered_map.hpp>

namespace Safir
{
namespace Dob
{
namespace Internal
{


    struct DistributionChannelData
    {
        boost::uint64_t includedNodes;
        std::string multicastAddress;
        DistributionChannelData(const boost::uint64_t nodes, const std::string & address):
            includedNodes(nodes),multicastAddress(address){}
    };
    typedef std::map<int,DistributionChannelData> DistributionChannelTable;


    class QualityOfServiceData:
        private boost::noncopyable
    {
    protected:
        static const int LOCAL_DISTRIBUTION_CHANNEL = -1;
        static const int SYSTEM_DISTRIBUTION_CHANNEL = 64;
        static const int POOL_DISTRIBUTION_CHANNEL = 65;
    public:
        QualityOfServiceData();
        ~QualityOfServiceData();

        void Init(); //throws ConfigurationError if something is not right

        void GetQualityOfServiceInfo(const Dob::Typesystem::TypeId typeId, //in
                                     int & distributionChannel,        //out
                                     int & priority,                   //out
                                     bool & isAcked) const;            //out

        void GetQualityOfServiceInfoForInternalDistribution(int & distributionChannel,        //out
                                                            int & priority,                   //out
                                                            bool & isAcked) const;            //out

        void GetQualityOfServiceInfoForPoolDistribution(int & distributionChannel,        //out
                                                        int & priority,                   //out
                                                        bool & isAcked) const;            //out

        int GetClosestPriority(const int    priority,
                               const bool   acked) const;

        inline bool IsLocal(const int distributionChannel) const
        {return distributionChannel == LOCAL_DISTRIBUTION_CHANNEL;}

        inline bool IsStandalone() const {return m_bIsStandalone;}

        const DistributionChannelTable & GetDistributionChannelTable() const {return m_DistributionChannelTable;}

        bool IsNodeInDistributionChannel(const Dob::Typesystem::TypeId typeId, const NodeNumber node) const;

        bool IsPriorityAcked(const int priority) const {return m_AckedPriorities[priority];}
    protected:
        void ReadParameters();
        void ReadPriorities();
        void ReadPriorityAliases();
        void ReadDistributionChannels();
        void ReadDistributionChannelAliases();

        int PriorityFromName(const std::wstring & name);
        int DistributionChannelFromName(const std::wstring & name);
        int GetPriority(const Dob::Typesystem::TypeId typeId);
        int GetDistributionChannel(const Dob::Typesystem::TypeId typeId);

        typedef std::vector<bool> AckedPriorities;
        struct QoSData
        {
            int distributionChannel;
            int priority;
            QoSData(const int channel, const int prio):distributionChannel(channel),priority(prio) {}
        };
        typedef boost::unordered_map<Dob::Typesystem::TypeId,QoSData> QoSTable;

        typedef std::map<std::wstring,int> NameIdTable;



        bool m_bIsStandalone;

        NameIdTable m_PriorityNameTable;
        AckedPriorities m_AckedPriorities;

        NameIdTable m_DistributionChannelNameTable;
        DistributionChannelTable m_DistributionChannelTable;

        QoSTable m_QoSTable;
    };
}
}
}
#endif

