/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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

#ifndef __TESTCASEREADER_H__
#define __TESTCASEREADER_H__

#include <DoseTest/Items/TestCase.h>
#include <boost/noncopyable.hpp>
#include <boost/filesystem/path.hpp>


class TestCaseReader:
    private boost::noncopyable
{
public:
    static void Initialize(const boost::filesystem::path & testCaseDir);

    static TestCaseReader & Instance();
    int NumberOfTestCases() const {return static_cast<int>(m_testCases.size());}

    DoseTest::Items::TestCasePtr GetTestCase(const int which);
private:
    TestCaseReader(const boost::filesystem::path & testCaseDir);
    ~TestCaseReader();

    std::vector<DoseTest::Items::TestCasePtr> m_testCases;

    static TestCaseReader * m_instance;
};

#endif

