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

#include <windows.h>
#include <stdio.h>
#include "resource.h"

typedef unsigned _int64 ulong64;
#include "../Defs/DoseNodeStatus.h"

DOSE_SHARED_DATA_S *G_pShm = NULL;

extern int G_Mode_Local_Remote; // 'L', 'R', 0

typedef unsigned _int64 ulong64;

//------------------------------------------------------------
// APIs that we link against dynamically in case they aren't
// present on the system we're running on.
//-------------------------------------------------------------

typedef DOSE_SHARED_DATA_S * (*PFGET_NODESHAREDDATA_POINTER)(void);

PFGET_NODESHAREDDATA_POINTER pfGet_NodeSharedData_Pointer;

typedef void (*PFDOSECOM_TEST)(DWORD TestCode, DWORD Param);

PFDOSECOM_TEST  pfDoseCom_Test;

/*******************************************************
*
********************************************************/
BOOL Load_DoseComDll(void)
{
    static HMODULE hModIpHlpAPI = 0;


    if ( !hModIpHlpAPI )
        hModIpHlpAPI = LoadLibrary( "dose_com.dll" );

    if(hModIpHlpAPI == NULL)
    {
        MessageBox(NULL,"Can not load dose_com.dll. Running in remote mode",
                "Error", MB_OK);
        G_Mode_Local_Remote = 'R';
        return(FALSE);
    }

    pfGet_NodeSharedData_Pointer
        = (PFGET_NODESHAREDDATA_POINTER) GetProcAddress(hModIpHlpAPI,
            "GetNodeSharedDataPointer");

    G_pShm = pfGet_NodeSharedData_Pointer();

    pfDoseCom_Test
        = (PFDOSECOM_TEST) GetProcAddress(hModIpHlpAPI, "DoseCom_Test");

    if(pfDoseCom_Test == NULL)
    {
        MessageBox(NULL,"Can not load pfDoseCom_Test().",
                "Error", MB_OK);
        return(FALSE);
    }

    return TRUE;
}

/*******************************************
*
********************************************/
static void info_1(HWND hWnd)
{
    int     pos;
    DWORD   WillStartSoon;
    char    buff[4096];
    char    MyIpAddrBuff[48];
    char    IpMcAddrBuff[48];

    if(G_Mode_Local_Remote == 'R')
    {
        SetDlgItemText(hWnd, IDC_EDIT_INFO,
            "Can not do this in Remote mode");
        return;
    }
    G_pShm = pfGet_NodeSharedData_Pointer();

    strcpy(MyIpAddrBuff,inet_ntoa( *(struct in_addr *)
                            &G_pShm->MyIpAddr_nw));

    strcpy(IpMcAddrBuff,inet_ntoa( *(struct in_addr *)
                            &G_pShm->IpMulticastAddr_nw));

    // G_pShm->PoolDistributionWillStartSoon holds time from GetTickCount()
    // So I show expired time time instead of absolute time in ticks
    if(G_pShm->PoolDistributionWillStartSoon)
        WillStartSoon = GetTickCount() - G_pShm->PoolDistributionWillStartSoon;
    else
        WillStartSoon = 0;

    sprintf(buff,
            "My IpAddr         = %s\r\n"
            "IP MulticastAddr  = %s\r\n"
            "IP NetAddr        = %s\r\n"
            "New               = %08X  %08X\r\n"
            "Up                = %08X  %08X\r\n"
            "Down              = %08X  %08X\r\n"
            "ToBePoolDistr     = %08X  %08X\r\n"
            "BeingPoolDistr    = %08X  %08X\r\n"
            "LatestPoolDistr   = %08X  %08X\r\n\r\n"
            "PoolDistributionWillStartSoon = %u ms has expired\r\n"
            "PoolDistributionIsInProgress  = %d\r\n"
            "PoolDistributionWillEndSoon   = %d\r\n\r\n"
            ,
            MyIpAddrBuff,IpMcAddrBuff,
            inet_ntoa( *(struct in_addr *)
                            &G_pShm->NetAddr_nw),
            (ulong)(G_pShm->BitMapNodesNew64>>32),
            (ulong)(G_pShm->BitMapNodesNew64 & 0xFFFFFFFF),
            (ulong)(G_pShm->BitMapNodesUp64>>32),
            (ulong)(G_pShm->BitMapNodesUp64 & 0xFFFFFFFF),
            (ulong)(G_pShm->BitMapNodesDown64>>32),
            (ulong)(G_pShm->BitMapNodesDown64 & 0xFFFFFFFF),
            (ulong)(G_pShm->BitMapToBePoolDistributed64>>32),
            (ulong)(G_pShm->BitMapToBePoolDistributed64 & 0xFFFFFFFF),
            (ulong)(G_pShm->BitMapBeingPoolDistributed64>>32),
            (ulong)(G_pShm->BitMapBeingPoolDistributed64 & 0xFFFFFFFF),
            (ulong)(G_pShm->BitMapLatestPoolDistributed64>>32),
            (ulong)(G_pShm->BitMapLatestPoolDistributed64 & 0xFFFFFFFF),
            WillStartSoon,
            G_pShm->PoolDistributionIsInProgress,
            G_pShm->PoolDistributionWillEndSoon);

    pos = strlen(buff);
    sprintf(&buff[pos],
            "Statistics:\r\n"
            "Tot ReceiveCount      = %d\r\n"
            "Tot TransmitCount     = %d\r\n"
            "ReTransmitCount       = %d\r\n"
            "LostAckCount          = %d\r\n"
            //"BuffOverFlowCount     = %d\r\n"
            "ReceiveQueueFullCount = %d\r\n"
            "TransmitQueueFullCount= %d\r\n"
            ,
            G_pShm->Statistics.TotRxCount,
            G_pShm->Statistics.TotTxCount,
            G_pShm->Statistics.ReTxCount,
            G_pShm->Statistics.LostAckCount,
            G_pShm->Statistics.ReceiveQueueFullCount,
            G_pShm->Statistics.TransmitQueueFullCount);

    // add more here ???????

    SetDlgItemText(hWnd, IDC_EDIT_INFO, buff);
}
/*---------------- end info_1() -------------------*/

/*******************************************
*
********************************************/
static void info_1_Help(HWND hWnd)
{
    //int       pos;
    char    buff[1024];


    sprintf(buff,
            "My IpAddr         = used Ip Adress\r\n"
            "IP MulticastAddr  = used Ip Multicast address\r\n"
            "IP NetAddr        = used to select IpAdress\r\n"
            "New               = bitmap for nodes that are detected but not has sent PoolDistribution\r\n"
            "Up                = bitmap for nodes that are OK and has sent PoolDistribution\r\n"
            "Down              = bitmap for nodes that has stopped\r\n"
            "ToBePoolDistr     = bitmap for nodes that shall be PoolDistributed soon\r\n"
            "BeingPoolDistr    = bitmap for nodes that currently are PoolDistributed\r\n"
            "LatestPoolDistr   = bitmap for latest or current 'ToBePoolDistr'\r\n"
            "PoolDistributionWillStartSoon = timer in ms since this condition started\r\n"
            "PoolDistributionIsInProgress  = PoolDistribution is in progress\r\n"
            "PoolDistributionWillEndSoon   = PoolDistribution will end when all pending messages are sent\r\n\r\n");

/*----
    pos = strlen(buff);

    sprintf(&buff[pos],
            "Statistics:\r\n"
            "Tot ReceiveCount      = \r\n"
            "Tot TransmitCount     = \r\n"
            "ReTransmitCount       = \r\n"
            "LostAckCount          = \r\n"
            "ReceiveQueueFullCount = \r\n"
            "TransmitQueueFullCount= \r\n");
----*/
    // add more here ???????

    SetDlgItemText(hWnd, IDC_EDIT_INFO, buff);
}

/*******************************************
*
********************************************/
static void SetDebugLevelInDoseComDll(HWND hWnd, char inc_dec)
{
    DWORD DebugLevel;
    char    buff[48];

    if(G_Mode_Local_Remote == 'R')
    {
        SetDlgItemText(hWnd, IDC_EDIT_INFO,
            "Can not do this in Remote mode");
        return;
    }

    pfDoseCom_Test('d', (DWORD) &DebugLevel); //Get current
    if(inc_dec == '+')
        DebugLevel++;
    else
    {
        if(DebugLevel>0) DebugLevel--;
    }
    pfDoseCom_Test('D', (DWORD) &DebugLevel); //Set new
    sprintf(buff,"Debug level = %d\r\n\r\n",DebugLevel);
    SetDlgItemText(hWnd, IDC_EDIT_INFO, buff);
}


//*******************************************************************
//
//*******************************************************************

static BOOL CALLBACK Hardware_Dialog(HWND hDlg, UINT message,
                                    WPARAM wParam,
                                    LPARAM) // lParam)
{
    RECT    rc;


    switch(message)
    {
        case WM_CREATE:
            break;

        case WM_INITDIALOG:
            //fall through

        case WM_SIZE:
            GetClientRect( hDlg, &rc );
            SetWindowPos(GetDlgItem(hDlg, IDC_EDIT_INFO),HWND_TOPMOST,
                        5, 30,
                        rc.right - rc.left - 10,
                        rc.bottom - rc.top - 35,
                        SWP_NOZORDER);
            return(TRUE);

        case WM_COMMAND:
        {
            switch(LOWORD(wParam))
            {
                case IDC_BUTTON_HELP:
                    info_1_Help(hDlg);
                    //SetDlgItemText(hDlg, IDC_EDIT_INFO,
                    //  "To be\r\nimplemeted");
                    break;

                case IDC_BUTTON_GETINFO:
                    info_1(hDlg);
                    return(TRUE);

                case IDC_BUTTON_DEBUG_PLUS:
                    SetDebugLevelInDoseComDll(hDlg, '+');
                    return(TRUE);

                case IDC_BUTTON_DEBUG_MINUS:
                    SetDebugLevelInDoseComDll(hDlg, '-');
                    return(TRUE);

                case IDCANCEL:
                    DestroyWindow(hDlg);
                    return(TRUE);

                default:
                    return(TRUE);
            }
            default:
                break;
        }
    }
    return(FALSE);
}
/*****************************************************#
* The one and only entry to this module
******************************************************/
void Start_GetInfoDlg(HINSTANCE hInst, HWND hDlg)
{
    CreateDialog(   hInst,
                    MAKEINTRESOURCE(IDD_DIALOG_INFO),
                    hDlg, Hardware_Dialog);
}
/*--------------------------- end DoseMonitorInfo.cpp ---------------------*/
