/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Lars Engdahl / stlsen
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

class CDoseComTransmit
{

public:
    int Xmit_Init(unsigned short DoseId);

    int Xmit_Msg(const char *pMsg, dcom_ulong32 MsgLength,
                 dcom_uchar8 PoolDistribution, dcom_uchar8 bUseAck,
                 int Priority, int   Destination);

    void Set_PoolDistributionIsCompleted(int Priority, int DestinationId);
    static void Get_Info(char *pBuf);

};

