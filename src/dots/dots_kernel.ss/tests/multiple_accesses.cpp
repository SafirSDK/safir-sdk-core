/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / stlrha
*
******************************************************************************/
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <boost/bind.hpp>

#if defined _MSC_VER
#  pragma warning (push)
#  pragma warning (disable : 4100)
#endif

#include <boost/thread.hpp>

#if defined _MSC_VER
#  pragma warning (pop)
#endif

void access()
{
    DotsC_NumberOfTypeIds();
}

int main(int argc, char* argv[])
{
    if (argc == 2 && std::string(argv[1]) == "sleep")
    {
        DotsC_NumberOfTypeIds();
        boost::this_thread::sleep_for(boost::chrono::seconds(1));
    }
    boost::thread_group tg;
    for (int i = 0; i < 50; ++i)
    {
        tg.create_thread(boost::bind(access));
    }
    tg.join_all();
    return 0;
}


