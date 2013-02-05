/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
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
#ifndef __DOSECOM_STUFF_H__
#define __DOSECOM_STUFF_H__

#include <Safir/Dob/Internal/DoseComAux/DosePlatform.h>
#include <Safir/Dob/Internal/DoseComAux/DoseNodeStatus.h>

DOSE_SHARED_DATA_S * Get_NodeSharedData_Pointer(void);

void IpAddr_ToString(unsigned long IpAddr_nw, char *StrIpAddr);

int Get_Status_Info(DOSE_SHARED_DATA_S *pShm, char *buff);

//Some stuff to avoid warning from moc
#include <QObject>

class dummy : public QObject{
    Q_OBJECT
};
#endif
