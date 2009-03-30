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

#define MAX_NUM_DEST_CHANNELS   32  // IpM channels
#define MAX_NUM_PRIO_CHANNELS   6
#define MAX_NUM_NODES           64  // must be 32 - 64
class CConfig
{
public:
    CConfig();

public:
    static int Dose_Config(unsigned char DoseId,
                        const char* multicastAddress,
                        const char* netAddress);

    static unsigned short   m_Dose_Port_Data;     // IpMc port for data
    static unsigned short   m_Dose_Port_Ack;      // UDP  port for ACK
    static unsigned short   m_Dose_KeepAlivePort; // KeepAlive
    static unsigned char    m_MyDoseId;
    static unsigned char    m_spare1;
    static unsigned long    m_MyIpAddr_nw;
    static unsigned long    m_BaseIpMultiCastAddr_nw;
    static unsigned long    m_NetAddr_nw;
    static ulong64          m_BitMapDestChannelMembers64[MAX_NUM_DEST_CHANNELS];

private:
    typedef struct
    {
        unsigned long   IpMulticastAddr_nw;
    } DEST_CHAN_S;

    static DEST_CHAN_S m_DestChannel[MAX_NUM_DEST_CHANNELS];

    static unsigned long m_UnicastIpAddr_nw[MAX_NUM_NODES];

public:
    static unsigned long Get_DestinationIpAddress(int DestinationId);

    static unsigned long Get_BaseIpMulticastAddress(void);

    static void Add_UnicastIpAddr(int DoseId, unsigned long IpAddr_nw);

    static int Add_DestinationId(int     DestinationId,
                                const char *IpMulticastAddr,
                                ulong64 BitMapDestChanMembers64);

    static int GetDestinationItem(int ix,
                                unsigned long *pIpMulticastAddr_nw,
                                char *pIsUsedForReception);

    static void Get_Info(int Mode, char *pBuff);
};
