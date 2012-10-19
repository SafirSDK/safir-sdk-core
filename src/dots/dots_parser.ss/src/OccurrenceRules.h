/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / joot
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
#ifndef __DOTS_PARSER_OCCURRENCE_RULES_H__
#define __DOTS_PARSER_OCCURRENCE_RULES_H__

#include <limits>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
    template <int Min, int Max>
    struct Occurrences
    {
        static const int MinOccurrences = Min;
        static const int MaxOccurrences = Max;

        Occurrences() : m_val(0) {}
        bool Valid() const {return m_val>=MinOccurrences && m_val<=MaxOccurrences;}
        void Reset() {m_val=0;}        
        void operator++() {++m_val;}
        bool operator!() {return !Valid();}
        int operator()() {return m_val;}

    private:
        int m_val;
    };

    typedef Occurrences<1, 1> One;
    typedef Occurrences<0, 1> OptionalOne;
    typedef Occurrences<1, INT_MAX> AtLeastOne;
    typedef Occurrences<0, INT_MAX> OptionalMany;
}
}
}

#endif
