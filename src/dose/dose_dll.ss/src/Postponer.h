/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safirsdkcore.com)
*
* Created by: Anders Widén / stawi
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

#ifndef __DOSE_POSTPONER_H__
#define __DOSE_POSTPONER_H__

#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#include <Safir/Dob/CallbackId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class Postponer:
        private boost::noncopyable
    {
    public:
        bool IsPostponed(const ConsumerId& consumer,
                         const Typesystem::TypeId typeId,
                         const CallbackId::Enumeration callbackId) const
        {
            return m_postponedTypes.find(boost::make_tuple(consumer,typeId, callbackId)) != m_postponedTypes.end();
        }

        void Postpone(const ConsumerId& consumer,
                      const Typesystem::TypeId typeId,
                      const CallbackId::Enumeration callbackId)
        {
            using namespace Safir::Dob::Typesystem;
            const TypeIdVector types = Operations::GetClassTree(typeId);
            for (TypeIdVector::const_iterator it = types.begin();
                 it != types.end(); ++it)
            {
                m_postponedTypes.insert(boost::make_tuple(consumer,*it, callbackId));
            }
        }

        void Clear()
        {
            m_postponedTypes.clear();
        }

    private:
        typedef std::set<boost::tuple<ConsumerId, Typesystem::TypeId, CallbackId::Enumeration> > PostponedTypes;
        PostponedTypes m_postponedTypes;
    };
}
}
}

#endif
