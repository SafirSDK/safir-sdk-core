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

class CDoseComReceive
{

public:
    int Read_Msg(dcom_ulong32 RxUseBitMap, dcom_ulong32 *pRxFromBitMap,
                  char **ppBuf, dcom_ulong32 *pSize, bool *pIsNative);

    int Receive_Init(dcom_ushort16 DoseId);

    static void Get_Info(char *pBuf);
    static void UpdateNodeUp(dcom_uchar8 DoseId);
};
