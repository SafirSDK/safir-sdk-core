/******************************************************************************
*
* Copyright Saab AB, 2013-2015 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#pragma once

#include <random>

namespace Safir
{
namespace Dob
{
namespace Internal
{
namespace Com
{
namespace Utilities
{
    class Random
    {
    public:
        Random(int min, int max)
            :m_engine(std::random_device()())
            ,m_distribution(min, max)
        {
        }

        int Get() const {return m_distribution(m_engine);}

    private:
        mutable std::mt19937 m_engine;
        mutable std::uniform_int_distribution<> m_distribution;
    };
}
}
}
}
}
