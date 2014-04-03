/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/RawStatistics.h>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include "MessageWrapperCreators.h"
#include <stdexcept>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "NodeStatisticsMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif


namespace
{
    void PrintMessage(const Safir::Dob::Internal::SP::NodeStatisticsMessage& msg, 
                      std::wostream& out, 
                      const unsigned int level = 0)
    {
        if (level > 1)
        {
            throw std::logic_error("Too many levels in NodeStatisticsMessage!");
        }

        const std::wstring indent = level == 0 ? L"" : L"    ";


        if (level == 0)
        {
            out << indent << msg.name().c_str() << "@" << msg.control_address().c_str() << " (" << msg.id() << ")";
        }

        for (int i = 0; i < msg.node_info_size(); ++i)
        {
            const auto& node = msg.node_info(i);
            out << "\n" << indent;
            if (node.is_dead())
            {
                out << "   [";
            }
            else
            {
                out << "    ";
            }
            out << node.name().c_str() 
                       << "@" << node.control_address().c_str() 
                       << " (" << node.id() << ")";
            if (node.is_dead())
            {
                out << "]";
            }
            else
            {
                out << "\n" << indent << "        recv = " << node.receive_count() << ", retransmit = " << node.retransmit_count();
            }
            
            if (node.has_remote_statistics())
            {
                PrintMessage(node.remote_statistics(), out, level + 1);
            }
            else if (level == 0 && !node.is_dead())
            {
                out << indent << "        [No remote statistics received yet]";
            }
        }
        out << std::flush;
    }
}

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{

    class RawStatistics::Impl
        : private boost::noncopyable
    {
    public:
        Impl(const NodeStatisticsMessage& message,
             const boost::shared_ptr<const NodeStatisticsMessage>& owner)
            : m_message(message)
            , m_owner(owner)
        {

        }

        const std::string& Name() const
        {
            return m_message.name();
        }

        boost::int64_t Id() const
        {
            return m_message.id();
        }

        const std::string& Address() const
        {
            return m_message.control_address();
        }
        
        int Size() const
        {
            return m_message.node_info_size();
        }

        const std::string& Name(const int index) const
        {
            return m_message.node_info(index).name();
        }

        boost::int64_t Id(const int index) const
        {
            return m_message.node_info(index).id();
        }

        const std::string& Address(const int index) const
        {
            return m_message.node_info(index).control_address();
        }

        bool MulticastEnabled(const int index) const
        {
            return m_message.node_info(index).multicast_enabled();
        }

        bool IsDead(const int index) const
        {
            return m_message.node_info(index).is_dead();
        }

        boost::uint32_t ReceiveCount(const int index) const
        {
            return m_message.node_info(index).receive_count();
        }

        boost::uint32_t RetransmitCount(const int index) const
        {
            return m_message.node_info(index).retransmit_count();
        }

        bool HasRemoteStatistics(const int index) const
        {
            return m_message.node_info(index).has_remote_statistics();
        }

        RawStatistics RemoteStatistics(const int index) const
        {
            if (m_message.node_info(index).has_remote_statistics())
            {
                return RawStatistics(boost::make_shared<Impl>(m_message.node_info(index).remote_statistics(), m_owner));
            }
            else
            {
                throw std::logic_error("No remote statistics available!");
            }
        }

        void Print(std::wostream& out) const
        {
            PrintMessage(m_message, out);
        }

    private:
        friend class RawStatisticsCreator;

        static RawStatistics Create(const boost::shared_ptr<NodeStatisticsMessage>& message)
        {
            return RawStatistics(boost::make_shared<Impl>(*message.get(), message));
        }

        const NodeStatisticsMessage& m_message;
        const boost::shared_ptr<const NodeStatisticsMessage> m_owner;
    };


    const std::string& RawStatistics::Name() const {return m_impl->Name();}
    boost::int64_t RawStatistics::Id() const {return m_impl->Id();}
    const std::string& RawStatistics::Address() const {return m_impl->Address();}
    
    int RawStatistics::Size() const {return m_impl->Size();}
    
    const std::string& RawStatistics::Name(const int index) const {return m_impl->Name(index);}
    boost::int64_t RawStatistics::Id(const int index) const {return m_impl->Id(index);}
    const std::string& RawStatistics::Address(const int index) const {return m_impl->Address(index);}
    bool RawStatistics::MulticastEnabled(const int index) const {return m_impl->MulticastEnabled(index);}
    
    bool RawStatistics::IsDead(const int index) const {return m_impl->IsDead(index);}
    boost::uint32_t RawStatistics::ReceiveCount(const int index) const {return m_impl->ReceiveCount(index);}
    boost::uint32_t RawStatistics::RetransmitCount(const int index) const {return m_impl->RetransmitCount(index);}
    
    bool RawStatistics::HasRemoteStatistics(const int index) const {return m_impl->HasRemoteStatistics(index);}
    RawStatistics RawStatistics::RemoteStatistics(const int index) const {return m_impl->RemoteStatistics(index);}
    
    void RawStatistics::Print(std::wostream& out) const {m_impl->Print(out);}

    RawStatistics RawStatisticsCreator::Create(const boost::shared_ptr<NodeStatisticsMessage>& message)
    {
        return RawStatistics::Impl::Create(message);
    }
}
}
}
}


