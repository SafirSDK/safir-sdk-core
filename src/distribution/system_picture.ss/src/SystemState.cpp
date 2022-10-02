/******************************************************************************
*
* Copyright Saab AB, 2013 (http://safirsdkcore.com)
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
#  pragma warning (push)
#  pragma warning (disable: 4127)
#endif

#include "SystemStateMessage.pb.h"

#ifdef _MSC_VER
#  pragma warning (pop)
#endif

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace SP
{
    void PrintMessage(const Safir::Dob::Internal::SP::SystemStateMessage& msg,
                      std::wostream& out)
    {
        out << "--------------------------------------------\n";
        out << "Election Id: " << msg.election_id() << (msg.is_detached()? " Detached!":"");
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

        bool IsDetached() const
        {
            return m_message->is_detached();
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

        std::wstring ToJson() const
        {
            std::wostringstream out;
            out <<  "{\"election_id\": " << m_message->election_id()
                << ", \"elected_id\": " << m_message->elected_id()
                << ", \"is_detached\": " << std::boolalpha << m_message->is_detached()
                << ", \"nodes\": [";
            for (int i = 0; i < m_message->node_info_size(); ++i)
            {
                const auto& node = m_message->node_info(i);
                out << (i==0?"{":",{") <<
                    "\"name\": \"" << node.name().c_str() << "\", "
                    "\"is_dead\": " << std::boolalpha << node.is_dead() << ", "
                    "\"id\": " << node.id() << ", "
                    "\"node_type_id\": " << node.node_type_id() << "}";
            }
            out << "]}";

            return out.str();
        }

    private:
        friend class SystemStateCreator;

        static SystemState Create(std::unique_ptr<SystemStateMessage> message)
        {
            std::sort(message->mutable_node_info()->begin(),
                      message->mutable_node_info()->end(),
                      [](const SystemStateMessage_NodeInfo& first,
                         const SystemStateMessage_NodeInfo& second){return first.id() < second.id();});
            return SystemState(std::make_shared<Impl>(std::move(message)));
        }

        std::unique_ptr<const SystemStateMessage> m_message;
    };

    SystemState::SystemState() {}

    void SystemState::CheckValid() const
    {
        if (m_impl == nullptr)
        {
            throw std::logic_error("Invalid use of default constructed SystemState object");
        }
    }

    int64_t SystemState::ElectedId() const {CheckValid(); return m_impl->ElectedId(); }
    int64_t SystemState::ElectionId() const {CheckValid(); return m_impl->ElectionId();}
    bool SystemState::IsDetached() const {CheckValid(); return m_impl->IsDetached();}

    int SystemState::Size() const
    {
        if (m_impl == nullptr)
        {
            return 0;
        }
        else
        {
            return m_impl->Size();
        }
    }

    const std::string& SystemState::Name(const int index) const {CheckValid(); return m_impl->Name(index);}
    int64_t SystemState::Id(const int index) const {CheckValid(); return m_impl->Id(index);}
    int64_t SystemState::NodeTypeId(const int index) const {CheckValid(); return m_impl->NodeTypeId(index);}
    const std::string& SystemState::ControlAddress(const int index) const {CheckValid(); return m_impl->ControlAddress(index);}
    const std::string& SystemState::DataAddress(const int index) const {CheckValid(); return m_impl->DataAddress(index);}
    bool SystemState::IsDead(const int index) const {CheckValid(); return m_impl->IsDead(index);}

    void SystemState::Print(std::wostream& out) const {CheckValid(); m_impl->Print(out);}
    std::wstring SystemState::ToJson() const {CheckValid(); return m_impl->ToJson();}

    SystemState SystemStateCreator::Create(std::unique_ptr<SystemStateMessage> message)
    {
        return SystemState::Impl::Create(std::move(message));
    }
}
}
}
}
