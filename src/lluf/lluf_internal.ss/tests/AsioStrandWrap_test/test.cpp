/******************************************************************************
*
* Copyright Saab AB, 2013-2022 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@gmail.com
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
#include "../../src/include/Safir/Utilities/Internal/AsioStrandWrap.h"
#include <iostream>
#include <functional>
#include <boost/asio.hpp>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

void Wait(int millis) {boost::this_thread::sleep_for(boost::chrono::milliseconds(millis));}

int main()
{
    boost::asio::io_context io;
    auto work=boost::asio::make_work_guard(io);
    boost::thread_group threads;
    for (int i = 0; i < 9; ++i)
    {
        threads.create_thread([&]{io.run();});
    }

    auto strand1 = boost::asio::io_context::strand(io);
    auto strand2 = boost::asio::make_strand(io);

    std::vector<bool> result;

    // Test 1 - this is not expected be called from correct strand. This is the reason we can't use bind_executor
    std::function<void()> f1 = boost::asio::bind_executor(strand1, [&]{ result.push_back(!strand1.running_in_this_thread()); });

    // Test 2
    auto f2 = boost::asio::bind_executor(strand1, [&]{ result.push_back(strand1.running_in_this_thread()); });

    // Test 3
    std::function<void()> f3 = Safir::Utilities::Internal::WrapInStrand(strand1, [&]{ result.push_back(strand1.running_in_this_thread()); });

    // Test 4
    auto f4 = Safir::Utilities::Internal::WrapInStrand(strand1, [&]{ result.push_back(strand1.running_in_this_thread()); });

    // Test 5
    std::function<void()> f5 = Safir::Utilities::Internal::WrapInStrand(strand2, [&]{ result.push_back(strand2.running_in_this_thread()); });

    // Test 6
    auto f6 = Safir::Utilities::Internal::WrapInStrand(strand2, [&]{ result.push_back(strand2.running_in_this_thread()); });

    boost::asio::dispatch(io, f1);
    Wait(200);
    boost::asio::dispatch(io, f2);
    Wait(200);
    boost::asio::dispatch(io, f3);
    Wait(200);
    boost::asio::dispatch(io, f4);
    Wait(200);
    boost::asio::dispatch(io, f5);
    Wait(200);
    boost::asio::dispatch(io, f6);

    work.reset();
    threads.join_all();

    int testNumber = 0;
    bool failed = false;
    for (auto ok : result)
    {
        ++testNumber;

        if (!ok)
        {
            failed = true;
            std::wcout << L"Test " << testNumber << L" failed!f" << std::endl;
        }
    }

    if (failed)
    {
        exit(1);
    }

    std::wcout << L"AsioStrandWrap_test passed!" << std::endl;

    return 0;
}
