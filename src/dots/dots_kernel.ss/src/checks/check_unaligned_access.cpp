/******************************************************************************
*
* Copyright Saab Systems AB, 2012 (http://www.safirsdk.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
*
******************************************************************************/
#include <boost/cstdint.hpp>
#include <iostream>

int main()
{
    char data [100] = {9,9,9,3,34,13,3,4,4,123,4,4,12,34,123,4,4,134,1,234,12,34,123};
    const boost::int32_t i = *reinterpret_cast<const boost::int32_t*>(data + 1 + (rand() % 3));
    const boost::int64_t j = *reinterpret_cast<const boost::int64_t*>(data + 1 + (rand() % 3));
    const float k = *reinterpret_cast<const float*>(data + 1 + (rand() % 3));
    const double l = *static_cast<const double*>(static_cast<void*>(data + 1 + (rand() % 3)));

    std::wcout << i << std::endl;
    std::wcout << j << std::endl;
    std::wcout << k << std::endl;
    std::wcout << l << std::endl;
    return 0;
}


