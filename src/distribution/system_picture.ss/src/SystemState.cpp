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
#include <Safir/Dob/Internal/SystemState.h>
#include <boost/noncopyable.hpp>
#include <boost/make_shared.hpp>
#include "MessageWrapperCreators.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4244)
#pragma warning (disable: 4127)
#endif

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#pragma warning (pop)
#endif


namespace
{
    void PrintMessage(const Safir::Dob::Internal::SP::SystemStateMessage& msg, 
                      std::wostream& out)
    {
        //TODO: print message
        /*        out << msg.name().c_str() << "@" << msg.control_address().c_str() << " (" << msg.id() << ")";

        for (int i = 0; i < msg.node_info_size(); ++i)
        {
            const auto& node = msg.node_info(i);
            out << "\n" 
                << "    " 
                << node.name().c_str() 
                << "@" << node.control_address().c_str() 
                << " (" << node.id() << ")";

            //out << "\n" << indent << "        recv = " << node.receive_count() << ", retransmit = " << node.retransmit_count();
        }
        out << std::flush;*/
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

    class SystemState::Impl
        : private boost::noncopyable
    {
    public:
        Impl(const SystemStateMessage& message,
             const boost::shared_ptr<const SystemStateMessage>& owner)
            : m_message(message)
            , m_owner(owner)
        {

        }

        boost::int64_t ElectedId() const
        {
            return m_message.elected_id();
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

        void Print(std::wostream& out) const
        {
            PrintMessage(m_message, out);
        }

    private:
        friend class SystemStateCreator;

        static SystemState Create(const boost::shared_ptr<SystemStateMessage>& message)
        {
            return SystemState(boost::make_shared<Impl>(*message.get(), message));
        }

        const SystemStateMessage& m_message;
        const boost::shared_ptr<const SystemStateMessage> m_owner;
    };


    boost::int64_t SystemState::ElectedId() const {return m_impl->ElectedId();}
    
    int SystemState::Size() const {return m_impl->Size();}
    
    const std::string& SystemState::Name(const int index) const {return m_impl->Name(index);}
    boost::int64_t SystemState::Id(const int index) const {return m_impl->Id(index);}
    const std::string& SystemState::Address(const int index) const {return m_impl->Address(index);}
    bool SystemState::MulticastEnabled(const int index) const {return m_impl->MulticastEnabled(index);}
    
    
    void SystemState::Print(std::wostream& out) const {m_impl->Print(out);}
    
    SystemState SystemStateCreator::Create(const boost::shared_ptr<SystemStateMessage>& message)
    {
        return SystemState::Impl::Create(message);
    }
}
}
}
}


