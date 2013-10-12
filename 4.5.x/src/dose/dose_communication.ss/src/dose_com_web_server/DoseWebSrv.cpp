/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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
#include <time.h>
#include "../Defs/DoseUdpMsg.h"
#include "TinyWebSrv.h"
#include "IpmSocket.h"

unsigned short DosePort = 6970; // See DoseComConfig.cpp
unsigned int   HttpPort = 88;

static int Debug = 0;

static CTinyWebSrv *g_pTinyWebSrv; // implements a tiny webserver
CIpmSocket ReqSock;

extern int start_web(void);

static HWND g_hWndMain = NULL;
static HINSTANCE g_hInstance = NULL;

static char g_CurrDirName[300] = {{0}};

/***************************************
*
****************************************/
static void Write_Msg(int WinNum, char *pTxt)
{
    if(WinNum==1)
        SetDlgItemText(g_hWndMain, IDC_EDIT_MSG_1, pTxt);
    else
        SetDlgItemText(g_hWndMain, IDC_EDIT_MSG_1, pTxt);
}
/**********************************************************
* Args as printf
***********************************************************/

void PrintLogMsg( const char *format, ... )
{
    va_list marker;
    char    buffer[1024];

    va_start( marker, format );     /* Initialize variable arguments. */

    _vsnprintf(buffer, sizeof(buffer), format, marker);

    va_end( marker ); /* needed ? Reset variable arguments */


    SetDlgItemText(g_hWndMain, IDC_EDIT_MSG_2, buffer);
}


/**************************************************************
* Send request - Get response
***************************************************************/
static int Send_Request(DWORD IpAddr_nw, char Cmd1, char Cmd2, char *buff)
{
    int     result;
    DOSE_UDP_GETINFO_MSG ReqMsg;
    static int bFirstTime = 1;
    ulong   MyIpAddr_nw;

    if(bFirstTime)
    {
        bFirstTime = 0;
        result =ReqSock.CIpmSocket::CreateIpMulticastSocket(
                            1,1, // bForReceive + bForSend,
                            0,  //IpMulticastAddr_nw,
                            9999, //    Port,
                            0,
                            1000); //   Opt_So_RcvTimeo_Timeout)
    }

    // If multihomed, get response addr that best matches destination address
    MyIpAddr_nw = ReqSock.GetOwnIpAddress(IpAddr_nw);

    ReqMsg.Magic        = DOSE_MSG_MAGIC;       // to be sure it is note some junk msg
    ReqMsg.MsgType      = MSG_TYPE_GETINFO_1;
    ReqMsg.DoseIdFrom   = 0;
    ReqMsg.IpAddrFrom_nw = MyIpAddr_nw; // Senders IpAddr
    ReqMsg.RespPort_nw  = htons(9999);
    ReqMsg.ReqCode_1    = Cmd1;
    ReqMsg.ReqCode_2    = Cmd2;

    //printf("Send request to %X - %c%c\n", htonl(IpAddr_nw), Cmd1, Cmd2);

    ReqSock.SendTo2(IpAddr_nw, DosePort,
                    (char *) &ReqMsg, sizeof(ReqMsg), NULL, 0);

    result = ReqSock.RecvFrom2(buff, 1200, NULL,0);

    //printf("Got response result = %d\n", result);

    if(result <= 0)
        sprintf(buff,"No response from %X\n", htonl(IpAddr_nw));
    else
        buff[result] = 0; // terminator
    return(result);
}

/****************************************************************
* pUrl = "127.0.0.1/xyz"
*****************************************************************/
static void SendRequestToThisIpAddr(char *pUrl,
                                    char *pBuff)
{
    int     jj;
    char    IpAddrStr[20];
    DWORD   IpAddr_nw;

    for(jj=0 ; jj< 20 ; jj++)
    {
        IpAddrStr[jj] = pUrl[jj];
        if(pUrl[jj] == '/') break;
    }
    if(pUrl[jj] != '/')
    {
        sprintf(pBuff,"Invalid syntax (missing '/'\n");
        return;
    }
    IpAddrStr[jj] = 0;

    IpAddr_nw = inet_addr(IpAddrStr);

    Send_Request(IpAddr_nw, pUrl[jj+1], pUrl[jj+2], pBuff);
}

/**************************************************
*
***************************************************/
static char *Get_Time(void)
{
    static char buff[20];
    time_t Time;
    struct tm *pTm;

    Time = time(NULL);
    pTm = localtime(&Time);

    sprintf(buff,"%02d:%02d:%02d\n",
                pTm->tm_hour, pTm->tm_min, pTm->tm_sec);
    return(buff);
}

/*******************************************************************
* Callback from TinyWebSrv when an accept connection is done
*
* http://127.0.0.1:8080/request.htm     ==> pUrl = "/request.htm"
* http://127.0.0.1:8080//request.htm    ==> pUrl = "//request.htm"
* http://127.0.0.1:8080/?request.htm    ==> pUrl = "/?request.htm"
* http://127.0.0.1:8080/?//127.0.0.1/AB ==> pUrl = "/?//127.0.0.1/AB"
*
*********************************************************************/

void WebCallBack(SOCKET SockId, HTTP_RX_HDR_S *pHttpHdr)
{
    static int b = 3; static int a = 7;
    char    buff[1500];


    sprintf(buff,"Callback: pUrl=<%s>", pHttpHdr->pUrl);

    PrintLogMsg(buff);

    if(pHttpHdr->pUrl[0] != '/') // Invalid
    {
        g_pTinyWebSrv->SendMsg(SockId, "Invalid cmd\n");
    }

    //=====================================================
    // If it NOT starts with ?, it is a local file. Get it.
    //
    // ???? here we should add a security protection:
    // Do NOT allow filenames that starts with:
    //  "."
    //  "X:"
    //=====================================================
    if(pHttpHdr->pUrl[1] != '?') // a file, pass file name
    {
        Write_Msg(1, pHttpHdr->pUrl);

        if((pHttpHdr->pUrl[1] == '.') || (pHttpHdr->pUrl[2] == ':'))
        {
            PrintLogMsg("This kind of URL is not allowed.");
            g_pTinyWebSrv->Send_HttpHeader(SockId, 400, ".htm");
            g_pTinyWebSrv->SendMsg(SockId,"<HTML><BODY>\r\n");
            g_pTinyWebSrv->SendMsg(SockId, "This kind of URL is not allowed.");
            g_pTinyWebSrv->SendMsg(SockId,"</BODY></HTML>");
            return;
        }

        if(pHttpHdr->pUrl[1] == 0)
            g_pTinyWebSrv->SendFile(SockId, "DoseInfo.htm");  // Default
        else
            g_pTinyWebSrv->SendFile(SockId, &pHttpHdr->pUrl[1]);
        return;
    }

    //========================================
    // This is a special request. "/?...."
    //
    // pUrl = "/?//127.0.0.1/AB"
    //========================================

    if((pHttpHdr->pUrl[2] == '/') && (pHttpHdr->pUrl[3] == '/'))
    {
        g_pTinyWebSrv->SendMsg(SockId,"%s\n", Get_Time());

        SendRequestToThisIpAddr(&pHttpHdr->pUrl[4], buff);
        g_pTinyWebSrv->SendMsg(SockId, buff);
        return;
    }

    if(pHttpHdr->pUrl[2] == 'A')
    {
        g_pTinyWebSrv->Send_HttpHeader(SockId, 200, ".htm");
        g_pTinyWebSrv->SendMsg(SockId,"<HTML><BODY>\r\n");
        g_pTinyWebSrv->SendMsg(SockId, "::%u::%u::", a++, b++);
        g_pTinyWebSrv->SendMsg(SockId,"</BODY></HTML>");
    }
    //else
    //if(pHttpHdr->pUrl[2] == 'e')
    //{
    //  //g_pTinyWebSrv->Send_HttpHeader(SockId, 200);
    //  //g_pTinyWebSrv->SendMsg(SockId,"<HTML><BODY>\r\n");
    //  g_pTinyWebSrv->SendMsg(SockId, "RxCount=%u\n", a++);
    //  g_pTinyWebSrv->SendMsg(SockId, "TxCount=%u\n", a++);
    //  g_pTinyWebSrv->SendMsg(SockId, "ErrCount=%u\n", a++);
    //  //g_pTinyWebSrv->SendMsg(SockId,"</BODY></HTML>");
    //}
    else
    if(pHttpHdr->pUrl[2] == 'E')
    {
        Send_Request(0x0100007F,'X','Y', buff);
        g_pTinyWebSrv->SendMsg(SockId, buff);
        //g_pTinyWebSrv->SendMsg(SockId, "TxCount=%u\n", a++);
        //g_pTinyWebSrv->SendMsg(SockId, "ErrCount=%u\n", a++);
    }
}

/*********************************************************
* The dialog procedure for the main window
*
**********************************************************/

static BOOL CALLBACK MainDlgProc( HWND hDlg,UINT msg,
                                WPARAM wParam,LPARAM lParam )
{
    char    *pMsg = NULL;
    char    buff[100];
    RECT    rc;

    switch ( msg )
    {
        case WM_INITDIALOG:
            g_hWndMain = hDlg;
            SetWindowPos(hDlg,NULL,10,20,0,0,
                SWP_NOOWNERZORDER | SWP_NOSIZE | SWP_NOZORDER);
            g_pTinyWebSrv = new CTinyWebSrv(HttpPort, WebCallBack);
            //start_web();
            sprintf(buff,"Started. WebSrv on port %d",HttpPort);
            Write_Msg(1, buff);

            // Change the icon for hwnd's window class.
            SetClassLong(hDlg, GCL_HICON,
                    (DWORD)LoadIcon(g_hInstance,MAKEINTRESOURCE(IDI_ICON1)));

            PrintLogMsg("Current dir =\r\n%s", g_CurrDirName);
            return TRUE;

        case WM_SIZE:
            GetClientRect( hDlg, &rc );
            SetWindowPos(GetDlgItem(hDlg, IDC_EDIT_MSG_2),HWND_TOPMOST,
                        5, 40,
                        rc.right - rc.left - 10,
                        rc.bottom - rc.top - 45,
                        SWP_NOZORDER);
            return(TRUE);

            break;

        case WM_CLOSE:
            EndDialog(hDlg, 0); break;

        case WM_COMMAND:
            switch(LOWORD(wParam))
            {
                case IDC_BUTTON_APPLY: //// Butt "Read ComPort"
                    //Do_Apply(hDlg);
                    break;

                //case  IDC_BUTT_HELP:
                //  MyDlgBox( NULL, gszHelpText,"Help LicoTest");
                //  break;
            }
            break;

        default:
            break;
    }
    return FALSE;
}
/*-------------------- end MainDlgProc() ------------------*/

/******************************************************
*
*
*******************************************************/

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpszCmdLine, int nCmdShow )
{
    g_hInstance = hInstance;

    // HTML files must be located on current dir.
    if(_strnicmp("Root=",lpszCmdLine,5) == 0)
    {
        strncpy(g_CurrDirName, &lpszCmdLine[5], sizeof(g_CurrDirName));
        SetCurrentDirectory(g_CurrDirName);   // new directory name
    }
    else
        GetCurrentDirectory(sizeof(g_CurrDirName), g_CurrDirName);

    // Bring up the user interface
    DialogBox(hInstance, MAKEINTRESOURCE(IDD_MOUSEHOOKCTRL),
                0, (DLGPROC) MainDlgProc );

    return 0;
}
/*--------------------- end DoseWebSrv.cpp.cpp ----------------*/
