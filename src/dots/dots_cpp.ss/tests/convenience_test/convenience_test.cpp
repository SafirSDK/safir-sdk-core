/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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

#include <Safir/Dob/Typesystem/Convenience.h>

#define BOOST_TEST_MODULE dots_convenience_test
#include <boost/test/unit_test.hpp>

namespace std
{
    //needed by boost test, and must be in std for it to work.
    std::ostream& operator <<(std::ostream& out, const std::wstring& str)
    {
        return out << Safir::Dob::Typesystem::Utilities::ToUtf8(str);
    }

    //needed by boost test, and must be in std for it to work.
    std::ostream& operator <<(std::ostream& out, const wchar_t* str)
    {
        return out << Safir::Dob::Typesystem::Utilities::ToUtf8(str);
    }
}

BOOST_AUTO_TEST_CASE(WstrTest)
{
    const auto res = Wstr("foo");
    BOOST_CHECK_EQUAL(res, L"foo");
}

BOOST_AUTO_TEST_CASE(StrTest)
{
    const auto res = Str(L"foo");
    BOOST_CHECK_EQUAL(res, "foo");
}


