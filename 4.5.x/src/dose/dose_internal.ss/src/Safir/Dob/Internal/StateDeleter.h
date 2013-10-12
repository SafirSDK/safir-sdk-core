/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_STATE_DELETER_H__
#define __DOSE_STATE_DELETER_H__

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <boost/interprocess/offset_ptr.hpp>
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <Safir/Dob/Internal/State.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class StateContainer;

    typedef boost::interprocess::offset_ptr<StateContainer> ThisPtr;

    class DOSE_INTERNAL_API StateDeleter :
        private SharedMemoryObject::my_deleter<State>
    {
    public:
        typedef SharedMemoryObject::my_deleter<State>::pointer pointer;

        StateDeleter(ThisPtr                _this,
                     Dob::Typesystem::Int64 key) : m_this(_this), m_key(key) {}

        void operator()(const pointer& p);

    private:
        ThisPtr                 m_this;
        Dob::Typesystem::Int64  m_key;
    };

}
}
}
#endif
