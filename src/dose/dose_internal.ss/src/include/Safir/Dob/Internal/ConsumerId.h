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

#ifndef _dose_internal_consumer_id_h
#define _dose_internal_consumer_id_h

#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <cstdint>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    struct DOSE_INTERNAL_API ConsumerId
    {
        ConsumerId() : consumer(0), lang(-1) {}

        ConsumerId(void* _consumer, short _lang) : consumer(_consumer), lang(_lang) {}
        ConsumerId(void* _consumer, std::int32_t _lang) : consumer(_consumer), lang(static_cast<short>(_lang)) {}

        bool operator<(const ConsumerId& other) const
        {
            if (lang == other.lang)
            {
                return consumer < other.consumer;
            }
            else
            {
                return lang < other.lang;
            }
        }

        bool operator==(const ConsumerId& other) const {return consumer == other.consumer && lang == other.lang;}
        bool operator!=(const ConsumerId& other) const {return !(*this == other);}

        void* consumer;
        short lang;
    };
}
}
}
#endif

