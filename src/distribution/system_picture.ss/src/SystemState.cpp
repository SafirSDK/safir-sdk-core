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
        out << "--------------------------------------------";

        for (int i = 0; i < msg.node_info_size(); ++i)
        {
            const auto& node = msg.node_info(i);
            out << "\n "
                << (node.is_dead() ? "D" : " ")
                << (msg.elected_id() == node.id() ? "E" : " ")
                << " "
                << node.name().c_str() 
                << "@" << node.control_address().c_str() 
                << " (id = " << node.id() << ", type = " << node.node_type_id() << ")";
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

    class SystemState::Impl
        : private boost::noncopyable
    {
    public:
        Impl(std::unique_ptr<SystemStateMessage> message)
            : m_message(std::move(message))
        {

        }

        int64_t ElectedId() const
        {
            return m_message->elected_id();
        }

        int64_t ElectionId() const
        {
            return m_message->election_id();
        }
        
        int Size() const
        {
            return m_message->node_info_size();
        }

        const std::string& Name(const int index) const
        {
            return m_message->node_info(index).name();
        }

        int64_t Id(const int index) const
        {
            return m_message->node_info(index).id();
        }

        int64_t NodeTypeId(const int index) const
        {
            return m_message->node_info(index).node_type_id();
        }

        const std::string& ControlAddress(const int index) const
        {
            return m_message->node_info(index).control_address();
        }

        const std::string& DataAddress(const int index) const
        {
            return m_message->node_info(index).data_address();
        }

        bool IsDead(const int index) const
        {
            return m_message->node_info(index).is_dead();
        }

        void Print(std::wostream& out) const
        {
            PrintMessage(*m_message, out);
        }

    private:
        friend class SystemStateCreator;

        static SystemState Create(std::unique_ptr<SystemStateMessage> message)
        {
            return SystemState(std::make_shared<Impl>(std::move(message)));
        }

        std::unique_ptr<const SystemStateMessage> m_message;
    };


    int64_t SystemState::ElectedId() const {return m_impl->ElectedId();}
    int64_t SystemState::ElectionId() const {return m_impl->ElectionId();}
    
    int SystemState::Size() const {return m_impl->Size();}
    
    const std::string& SystemState::Name(const int index) const {return m_impl->Name(index);}
    int64_t SystemState::Id(const int index) const {return m_impl->Id(index);}
    int64_t SystemState::NodeTypeId(const int index) const {return m_impl->NodeTypeId(index);}
    const std::string& SystemState::ControlAddress(const int index) const {return m_impl->ControlAddress(index);}
    const std::string& SystemState::DataAddress(const int index) const {return m_impl->DataAddress(index);}
    bool SystemState::IsDead(const int index) const {return m_impl->IsDead(index);}

    void SystemState::Print(std::wostream& out) const {m_impl->Print(out);}
    
    SystemState SystemStateCreator::Create(std::unique_ptr<SystemStateMessage> message)
    {
        return SystemState::Impl::Create(std::move(message));
    }
}
}
}
}


