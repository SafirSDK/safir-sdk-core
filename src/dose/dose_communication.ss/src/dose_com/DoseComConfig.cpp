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

#ifndef _CRT_SECURE_NO_DEPRECATE
#define _CRT_SECURE_NO_DEPRECATE
#endif
#define IS_USING_SOCKETS

#include "DosePlatform.h"

#include "DoseOsInterface.h"

#include "PrintError.h"
#include "DoseComConfig.h"
#include "IpmSocket.h"
#include <string.h>
#include <stdlib.h>


extern volatile int * volatile pDbg;

#define CONFIG_FILENAME "DoseCom.cfg"

//----------------------
// Default values
//---------------------

#define DOSE_PORT_DEFAULT               6970
#define DOSE_IPMULTICASTADDR_DEFAULT    "224.5.6.7"

static int g_bFirstTime = 1;

//===================================================
// The static members
//===================================================

unsigned short  CConfig::m_Dose_Port_Data;
unsigned short  CConfig::m_Dose_Port_Ack;
unsigned short  CConfig::m_Dose_KeepAlivePort;
unsigned char   CConfig::m_MyDoseId = 0xFF;
unsigned char   CConfig::m_spare1 = 0xFF;
unsigned long   CConfig::m_MyIpAddr_nw;
unsigned long   CConfig::m_BaseIpMultiCastAddr_nw = 0;
unsigned long   CConfig::m_NetAddr_nw = 0;

ulong64   CConfig::m_BitMapDestChannelMembers64[MAX_NUM_DEST_CHANNELS];

unsigned long CConfig::m_UnicastIpAddr_nw[64] = {0}; //MAX_NUM_NODES];

// This is index range 0-MAX_NUM_DEST_CHANNELS
CConfig::DEST_CHAN_S CConfig::m_DestChannel[MAX_NUM_DEST_CHANNELS];


/*************************************************************************
* Called at startup
* Gets configuration parameters
*
* Sets:                     Priority
* ------------------------------------------------------------------------
* m_BaseIpMultiCastAddr_nw  1) Cfg file 2) arg 3) DOSE_IPMULTICASTADDR_DEFAULT
* m_NetAddr_nw              1) Cfg file 2) arg 3) default = 0
* m_MyIpAddr_nw             Calculate using m_NetAddr_nw
* m_MyDoseId                1) Cfg file 2) arg
* m_Dose_KeepAlivePort   1) Cfg file 2) DOSE_PORT_DEFAULT;
* m_Dose_Port_Ack        m_Dose_KeepAlivePort+1;
* m_Dose_Port_Data       m_Dose_KeepAlivePort+2;
*
**************************************************************************/

int CConfig::Dose_Config(uchar DoseId,
                        const char* multicastAddress,
                        const char* netAddress)
{
    FILE    *pFile;
    unsigned long dwTmp;
    unsigned short  wTmp;
    char    *pEnvVar;
    char    line[256];
    char    FileName[200];

    // Set default values

    m_Dose_KeepAlivePort = DOSE_PORT_DEFAULT;
    m_Dose_Port_Ack      = DOSE_PORT_DEFAULT+1;
    m_Dose_Port_Data     = DOSE_PORT_DEFAULT+2;

    //if(m_BaseIpMultiCastAddr_nw == 0)

    if(multicastAddress != NULL)
        m_BaseIpMultiCastAddr_nw = DoseOs::Inet_Addr(multicastAddress);
    else
        m_BaseIpMultiCastAddr_nw = DoseOs::Inet_Addr(DOSE_IPMULTICASTADDR_DEFAULT);

    if(netAddress != NULL)
        m_NetAddr_nw = DoseOs::Inet_Addr(netAddress);
    else
        m_NetAddr_nw = 0;

    m_MyDoseId = DoseId;

    //----------------------------------
    // Open and Read configuration file
    // This might overwrite values
    //---------------------------------

    // try current dir
    strcpy(FileName,CONFIG_FILENAME);
    if ((pFile = fopen(FileName, "r")) != NULL)
        goto Read_the_config_file;

    // try using environment variable current dir
    pEnvVar = getenv( "DOSEDIR" );
    if(pEnvVar != NULL)
    {
        sprintf(FileName,"%s/%s", pEnvVar,CONFIG_FILENAME);
        if ((pFile = fopen(FileName, "r")) != NULL)
            goto Read_the_config_file;
    }

    //PrintErr(DoseOs::Get_LastError(),
    //  "Can not open DoseCom config file. Using default values.\n");

Read_the_config_file:

    if (pFile != NULL)
    {
        PrintErr(0,
            "Warning Using DoseCom config file to override parameters.\n");

        if(*pDbg) PrintDbg("Start reading from %s\n", FileName);
        while (fgets(line, 120, pFile) != NULL)
        {
            if ( (strlen(line) < 4) || (line[0] == '#')) continue;

            if(_strnicmp(line,"Port=",5) == 0)
            {
                wTmp = (ushort) atoi(&line[5]);
                m_Dose_Port_Data     = wTmp + 2;
                m_Dose_Port_Ack      = wTmp + 1;
                m_Dose_KeepAlivePort = wTmp;
                if(*pDbg) PrintDbg("Port D/A/K= %d/%d/%d\n", wTmp,wTmp+1,wTmp+2);
            }
            else
            if(_strnicmp(line,"NetAddr=",8) == 0)
            {
                dwTmp = DoseOs::Inet_Addr(&line[8]);
                m_NetAddr_nw = dwTmp;
                if(*pDbg)
                    PrintDbg("NetAdr=%s\n",
                        DoseOs::Inet_Ntoa(m_NetAddr_nw));
            }
            else
            if(_strnicmp(line,"IpmAddr=",8) == 0)
            {
                dwTmp = DoseOs::Inet_Addr(&line[8]);
                m_BaseIpMultiCastAddr_nw = dwTmp;
                if(*pDbg)
                    PrintDbg("McIP=%s\n",
                        DoseOs::Inet_Ntoa(m_BaseIpMultiCastAddr_nw));
            }
            else
            if(_strnicmp(line,"Debug=",6) == 0)
            {
                *pDbg = atoi(&line[6]);
                if(*pDbg) PrintDbg("Debug = %d\n", *pDbg);
            }
            else
            if(_strnicmp(line,"DoseId=",7) == 0)
            {
                m_MyDoseId = (uchar) atoi(&line[7]);
                if(*pDbg) PrintDbg("DoseId = %d\n", m_MyDoseId);
            }
            else
                PrintErr(0,"Invalid line in CFG file= <%s>\n",line);
        } // end while(fgets ...)
        fclose(pFile);
    }
    //---------------------------------------------------------------------
    // ???? Put data in shared memory so DoseMonitor can access it.
    //---------------------------------------------------------------------

    m_MyIpAddr_nw = CIpmSocket::Get_OwnIpAddress(m_NetAddr_nw);


    if(*pDbg)
        PrintDbg("MyIpAddr = %s Port=%d, (+1), (+2)\n",
                    DoseOs::Inet_Ntoa(m_MyIpAddr_nw),
                    m_Dose_Port_Data);
    return(0);
}
/*---------------------- end Dose_Config() --------------*/

/**********************************************************************
*
**********************************************************************/
int CConfig::Add_DestinationId( int DestinationId,
                    const char  *IpMulticastAddr,
                    ulong64 BitMapDestChanMembers64)
{
    int ix;


    // tested by caller
    //if((DestinationId < 64)
    //  || (DestinationId > (64+MAX_NUM_DEST_CHANNELS)))
    //{
    //  return(ERR_DOSECOM_INVALID_PARAM);
    //}

    if (g_bFirstTime)
    {
        g_bFirstTime = 0;

        for(int jj = 0; jj<MAX_NUM_DEST_CHANNELS ; jj++)
        {
            m_DestChannel[jj].IpMulticastAddr_nw = 0;
        }
        m_BaseIpMultiCastAddr_nw = 0;
    }

    ix = DestinationId - 64;


    m_DestChannel[ix].IpMulticastAddr_nw = DoseOs::Inet_Addr(IpMulticastAddr);

    if(*pDbg>1)
        PrintDbg("Add_DestinationId() %2d IP=%08X = %s\n",
                DestinationId,
                m_DestChannel[ix].IpMulticastAddr_nw,
                IpMulticastAddr);

    m_BitMapDestChannelMembers64[ix] = BitMapDestChanMembers64;

    return(0);
}
/*---------------- end Add_DestinationId() -----------------*/

/********************************************************
*
*********************************************************/
void CConfig::Add_UnicastIpAddr(int DoseId, unsigned long IpAddr_nw)
{
    m_UnicastIpAddr_nw[DoseId] = IpAddr_nw;
}
/**********************************************************************
*
* DestinationId is in range:
* 0-63 for Unicast
* 64 - (64+MAX_NUM_DEST_CHANNELS) for Multicast
*
**********************************************************************/
unsigned long CConfig::Get_DestinationIpAddress(int DestinationId)
{
    if(DestinationId < 0)
    {
        return(0xFFFFFFFF);
    }
    if(DestinationId < 64)
    {
        return m_UnicastIpAddr_nw[DestinationId];
    }

    if(DestinationId < (64+MAX_NUM_DEST_CHANNELS))
    {
        return m_DestChannel[DestinationId-64].IpMulticastAddr_nw;
    }
    return(0xFFFFFFFF);
}

/******************************************************
*
* Ix is in range 0 - MAX_NUM_DEST_CHANNELS
*******************************************************/
int CConfig::GetDestinationItem(int ix,
                                unsigned long *pIpMulticastAddr_nw,
                                char *pIsUsedForReception)
{
    if(ix >= MAX_NUM_DEST_CHANNELS) return(-1);

    if(m_DestChannel[ix].IpMulticastAddr_nw == 0) return(-1);

    *pIpMulticastAddr_nw = m_DestChannel[ix].IpMulticastAddr_nw;

    if(m_BitMapDestChannelMembers64[ix] & ((ulong64)1<<m_MyDoseId)) // if this bit is set
        *pIsUsedForReception = 1;
    else
        *pIsUsedForReception = 0;

    //PrintDbg("GetDestinationItem(%d, %X) %I64X Did=%d\n",
    //   ix, *pIpMulticastAddr_nw,m_BitMapDestChannelMembers64[ix], m_MyDoseId );

    return(0); //OK
}
/**********************************************************************
*
**********************************************************************/
unsigned long CConfig::Get_BaseIpMulticastAddress(void)
{
    return m_BaseIpMultiCastAddr_nw;
}
/******************************************************************
*
* Can consume 28 + MAX_NUM_DEST_CHANNELS * 21 chars = 28+32*21 = 700
* Mode = RequestCode_2
********************************************************************/

void CConfig::Get_Info(int, // Mode,
                       char *pBuff)
{
    int pos;


    sprintf(pBuff,
        "DestChanMembers DoseId %d\n", m_MyDoseId);

    for(int ChNum = 0 ; ChNum < MAX_NUM_DEST_CHANNELS ; ChNum++)
    {
        if(m_BitMapDestChannelMembers64[ChNum] != (ulong64) 0)
        {
            pos = strlen(pBuff);
            sprintf(&pBuff[pos], "%2X %08lX %08lX\n",ChNum,
                    (ulong)(m_BitMapDestChannelMembers64[ChNum]>>32),
                    (ulong)(m_BitMapDestChannelMembers64[ChNum] & 0xFFFFFFFF));
        }
    }
}
/*-------------------------------------- end DoseComConfig.cpp -------*/
