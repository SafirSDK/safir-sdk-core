/******************************************************************************
*
* Copyright Saab AB, 2012-2013 (http://safirsdkcore.com)
*
* Created by: Lars Hagström
*
******************************************************************************/
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <iostream>

int main(int,char**)
{
    bool success = true;

    if (Safir::Dob::Typesystem::Internal::DEFAULT_CHANNEL_ID != Safir::Dob::Typesystem::Internal::Generate64BitHash(L"DEFAULT_CHANNEL"))
    {
        success = false;
        std::wcout << "Safir::Dob::Typesystem::Internal::DEFAULT_CHANNEL_ID has incorrect value!" << std::endl;
    }

    if (Safir::Dob::Typesystem::Internal::DEFAULT_HANDLER_ID != Safir::Dob::Typesystem::Internal::Generate64BitHash(L"DEFAULT_HANDLER"))
    {
        success = false;
        std::wcout << "Safir::Dob::Typesystem::Internal::DEFAULT_HANDLER_ID has incorrect value!" << std::endl;
    }

    if (Safir::Dob::Typesystem::Internal::ALL_CHANNELS_ID != Safir::Dob::Typesystem::Internal::Generate64BitHash(L"ALL_CHANNELS"))
    {
        success = false;
        std::wcout << "Safir::Dob::Typesystem::Internal::ALL_CHANNELS_ID has incorrect value!" << std::endl;
    }

    if (Safir::Dob::Typesystem::Internal::ALL_HANDLERS_ID != Safir::Dob::Typesystem::Internal::Generate64BitHash(L"ALL_HANDLERS"))
    {
        success = false;
        std::wcout << "Safir::Dob::Typesystem::Internal::ALL_HANDLERS_ID has incorrect value!" << std::endl;
    }
    
    if (success)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}
