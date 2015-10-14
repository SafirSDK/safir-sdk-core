/******************************************************************************
*
* Copyright Saab AB, 2014-2015 (http://safirsdkcore.com)
*
* Created by: Anders Widén / anders.widen@consoden.se
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
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <boost/filesystem/path.hpp>
#include <iostream>
#define BOOST_TEST_MODULE ConfigHelperTest
#include <boost/test/unit_test.hpp>



namespace
{
    std::string GetEnv(const std::string& name)
    {
        char* env = getenv(name.c_str());
        if (env == NULL)
        {
            throw std::logic_error("Environment variable " + name + " is not set");
        }
        return env;
    }
}

BOOST_AUTO_TEST_CASE(GetDouDirectories)
{
    using namespace Safir::Utilities::Internal;
    ConfigReader reader;

    std::vector<std::pair<std::string,std::string> > douFilePaths = ConfigHelper::GetDouDirectories(reader);

    if (Safir::Utilities::Internal::Expansion::GetSafirInstance() == 0)
    {
        BOOST_REQUIRE_EQUAL (douFilePaths.size(), 4U);

        BOOST_REQUIRE_EQUAL(douFilePaths[0].first, "Default");
        BOOST_REQUIRE_EQUAL(douFilePaths[1].first, "Override");
        BOOST_REQUIRE_EQUAL(douFilePaths[2].first, "AnotherOverride");
        BOOST_REQUIRE_EQUAL(douFilePaths[3].first, "Instance0Override");

        const std::string curbindir = GetEnv("CMAKE_CURRENT_BINARY_DIR");
        if (douFilePaths[0].second != "/path/to/default/directory" ||
            douFilePaths[1].second != "/path/to/some/other/directory" ||
            (douFilePaths[2].second != curbindir + "/AnotherOverride" &&
             douFilePaths[2].second != curbindir + "\\AnotherOverride") ||
             douFilePaths[3].second != "/path/to/an/instance0/override/directory")
        {
            std::wcout << "Unexpected path!\n"
                       << " " << douFilePaths[0].second.c_str() << "\n"
                       << " " << douFilePaths[1].second.c_str() << "\n"
                       << " " << douFilePaths[2].second.c_str() << "\n"
                       << " " << douFilePaths[3].second.c_str() << std::endl;

            BOOST_FAIL("Unexpected path");
        }
    }
    else if (Safir::Utilities::Internal::Expansion::GetSafirInstance() == 1)
    {
        BOOST_REQUIRE_EQUAL (douFilePaths.size(), 4U);

        BOOST_REQUIRE_EQUAL(douFilePaths[0].first, "Default");
        BOOST_REQUIRE_EQUAL(douFilePaths[1].first, "Override");
        BOOST_REQUIRE_EQUAL(douFilePaths[2].first, "AnotherOverride");
        BOOST_REQUIRE_EQUAL(douFilePaths[3].first, "Instance1Override");

        const std::string curbindir = GetEnv("CMAKE_CURRENT_BINARY_DIR");
        if (douFilePaths[0].second != "/path/to/default/directory" ||
            douFilePaths[1].second != "/path/to/some/other/directory" ||
            (douFilePaths[2].second != curbindir + "/AnotherOverride" &&
             douFilePaths[2].second != curbindir + "\\AnotherOverride") ||
             douFilePaths[3].second != "/path/to/an/instance1/override/directory")
        {
            std::wcout << "Unexpected path!\n"
                       << " " << douFilePaths[0].second.c_str() << "\n"
                       << " " << douFilePaths[1].second.c_str() << "\n"
                       << " " << douFilePaths[2].second.c_str() << "\n"
                       << " " << douFilePaths[3].second.c_str() << std::endl;

            BOOST_FAIL("Unexpected path");
        }
    }
}

BOOST_AUTO_TEST_CASE(GetDouDependencies)
{
    using namespace Safir::Utilities::Internal;
    ConfigReader reader;

    BOOST_CHECK(ConfigHelper::GetDouDependencies(reader, "Default").empty());
    std::set<std::string> deps = ConfigHelper::GetDouDependencies(reader, "Override");
    BOOST_CHECK_EQUAL(deps.size(), 1U);
    BOOST_CHECK_EQUAL(*deps.begin(),"Default");
    deps = ConfigHelper::GetDouDependencies(reader, "AnotherOverride");
    BOOST_CHECK_EQUAL(deps.size(), 2U);
    BOOST_CHECK(deps.find("Override") != deps.end());
    BOOST_CHECK(deps.find("Default") != deps.end());
    BOOST_CHECK_THROW(ConfigHelper::GetDouDependencies(reader, "Klopp"), std::logic_error);
}
