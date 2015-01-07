/******************************************************************************
*
* Copyright Saab AB, 2011-2013 (http://safir.sourceforge.net)
*
* Created by: Mikael Wennerberg / stmiwn
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
#include <iostream>
#include <Safir/Dob/Internal/DoseTest/dose_test_util.h>
#if 0 //stewart
#include "dose_com_utils.h"
#endif
#include <Safir/Dob/Typesystem/Utilities.h>


void InhibitOutgoingTraffic(const bool inhibit, bool& success)
{
#if 0 //stewart
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();  

    if (pShm->MyIpAddr_nw == 0)
    {
        std::wcout << "No dose_main running!"  << std::endl;
        success = false;
        return;
    }

    pShm->InhibitOutgoingTraffic = inhibit;     
    success = true;
#endif
}


void InhibitOutgoingTrafficStatus(bool& isInhibited)
{
#if 0 //stewart
    DOSE_SHARED_DATA_S * pShm = (DOSE_SHARED_DATA_S *) Get_NodeSharedData_Pointer();  
    if (pShm->InhibitOutgoingTraffic == 0)
    {
        isInhibited = false;
    }
    else
    {
        isInhibited = true;
    }
#endif
}


