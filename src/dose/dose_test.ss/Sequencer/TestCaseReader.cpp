/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / stlrha
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

#include "TestCaseReader.h"
#include <boost/regex.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/convenience.hpp>
#include <boost/filesystem/fstream.hpp>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <iostream>


#ifdef _MSC_VER
  #pragma warning(push)
  #pragma warning(disable: 4702)
#endif

#include <boost/lexical_cast.hpp>

#ifdef _MSC_VER
  #pragma warning(pop)
#endif


//static member initialization
TestCaseReader * TestCaseReader::m_instance = NULL;


void
TestCaseReader::Initialize(const boost::filesystem::path & testCaseDir)
{
    assert(m_instance == NULL);
    m_instance = new TestCaseReader(testCaseDir);
}

TestCaseReader & TestCaseReader::Instance()
{
    assert(m_instance != NULL);
    return *m_instance;
}

TestCaseReader::TestCaseReader(const boost::filesystem::path & testCaseDir)
{
    const boost::regex expr("(\\d{3,4})-(.*)\\.xml");
    for (boost::filesystem::directory_iterator it = boost::filesystem::directory_iterator(testCaseDir);
         it != boost::filesystem::directory_iterator(); ++it)
    {
        const boost::filesystem::path path = it->path();
        //ignore files that don't end in ".xml"
        if (path.extension() != ".xml")
        {
            continue;
        }

#if defined (BOOST_FILESYSTEM_VERSION) && BOOST_FILESYSTEM_VERSION == 3
        const std::string filename = path.filename().string();
#else
        const std::string filename = path.filename();
#endif

        boost::smatch matchResults;

        if (boost::regex_match(filename,matchResults,expr))
        {
            //std::wcout << "Found testcase " << matchResults[1].str().c_str() << " brief description '" << matchResults[2].str().c_str()<< "'" << std::endl;
            const size_t tc = boost::lexical_cast<size_t>(matchResults[1]);

            if (m_testCases.size() < (tc + 1))
            {
                m_testCases.resize(tc + 1);
            }

            if (m_testCases[tc] != NULL)
            {
                std::wcerr << "There appears to be two test cases with number " << tc << std::endl;
                exit(1);
            }

            std::ostringstream xml;
            xml << boost::filesystem::ifstream(path).rdbuf();
            //std::wcout << "Read xml (" << xml.str().size() << " bytes) '" << xml.str().c_str() << "'" << std::endl;
            try
            {
                m_testCases[tc] = boost::dynamic_pointer_cast<DoseTest::Items::TestCase>
                    (Safir::Dob::Typesystem::Serialization::ToObject(Safir::Dob::Typesystem::Utilities::ToWstring(xml.str())));
            }
            catch (const std::exception & exc)
            {
                std::wcerr << "Failed to read file '" << path.string().c_str() << "' due to exception with message" << std::endl
                           <<exc.what() << std::endl;
                exit(2);
            }
        }
        else
        {
            std::wcerr << "File '"
                       << path.filename().c_str()
                       << "' did not match the pattern for test case files: '"
                       << expr.str().c_str()
                       << "'"  << std::endl;
        }
    }
}


DoseTest::Items::TestCasePtr
TestCaseReader::GetTestCase(const int which)
{
    if (m_testCases[which] == NULL)
    {
        return DoseTest::Items::TestCasePtr();
    }
    else
    {
        return boost::static_pointer_cast<DoseTest::Items::TestCase>(m_testCases[which]->Clone());
    }
}
