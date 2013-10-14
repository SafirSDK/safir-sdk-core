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

/*******************************************************************
* DoseMonitor.cpp - a part of DoseMonitor.exe
*
* Design:
* ---------
* WinMain()
*   if(IsDlgButtonChecked(hWnd,IDC_CHECK_WEBSRW) )  tmp = 1;
*************************************************************************/

#include <windows.h>
#include <stdio.h>
#include <process.h>
#include "resource.h"
#include "PrintError.h"

typedef unsigned _int64 ulong64;

#include "../Defs/DoseNodeStatus.h"


extern DWORD Get_RxCount(BYTE DoseId);
extern int Get_NodeStatus(BYTE DoseId, int *pUpDown, DWORD *pIpAddr_nw);
extern int Get_BitMapNodesNew(int ix,   unsigned _int64 *pBitMap);
extern int Get_BitMapNodesUp(int ix,   unsigned _int64 *pBitMap);
extern int Get_BitMapNodesDown(int ix, unsigned _int64 *pBitMap);
extern DWORD Get_MyIpAddr(void);


extern void Start_GetInfoDlg(HINSTANCE hInst, HWND hDlg);
extern BOOL Load_DoseComDll(void);
extern void Start_Log_Thread(void);

extern int G_Mode_Local_Remote; // 'L', 'R', 0

extern DOSE_SHARED_DATA_S *G_pShm;


//---------------------------
//
//---------------------------

int     Debug = 0;

static HWND g_hWnd = NULL;

static HINSTANCE g_hInstance;

#define POS_X0      10
#define POS_X1      100
#define POS_X_HDR   10
#define POS_Y_HDR   40
#define POS_Y_USR   60+10
#define POS_Y_PWD   90+10
#define POS_Y_HKPT  130+10
#define POS_Y_MSG   120

//#define IDC_TIMER1            1099

static BOOL g_IsRunning = FALSE;

#define Y_POS_CHECK     (30+29)

#define DELTA           19  // space between lines

#define X_POS_CIRCLE      5

#define X_POS_INDEX      25
#define WIDTH_INDEX      30

#define X_POS_IPADDR     (X_POS_INDEX+WIDTH_INDEX+2)
#define WIDTH_IPADDR    120

#define X_POS_STATUS    (X_POS_IPADDR+WIDTH_IPADDR+2)
#define WIDTH_STATUS     50

#define X_POS_RXCOUNT   (X_POS_STATUS+WIDTH_STATUS+2)
#define WIDTH_RXCOUNT    50

#define WIN_X1  0
#define WIN_Y1  0

#define WIN_X2 (X_POS_RXCOUNT + WIDTH_RXCOUNT + 15)

#define IDM_INDEX       500
#define IDM_IPADDR      600
#define IDM_STATUS      700
#define IDM_RXCOUNT     800

//--------------------------

#define IDM_STATIC      309

#define IDM_EXIT        105

static int      g_NumNodes = 6;

static BOOL bMyNodeIsShown = FALSE;
static unsigned _int64 g_BitMap_New64   = (unsigned _int64) 0;
static unsigned _int64 g_BitMap_Up64    = (unsigned _int64) 0;
static unsigned _int64 g_BitMapDown64   = (unsigned _int64) 0;

static DWORD g_SavedRxCount[64] = {0};

//#######################################################
// Section Graphical Dialog routines
//#######################################################

/*******************************************************
*
********************************************************/

static void Paint_it(HWND hWnd)
{
    PAINTSTRUCT Paint;
    HDC     hDC;
    int     Ypos = 0;
    int     jj;
    int     result;
    int     UpDown;
    DWORD   IpAddr_nw;
    static HBRUSH   myRedBrush;
    static HBRUSH   myGreenBrush;
    static HBRUSH   myYellowBrush;
    static HBRUSH   myBlueBrush;
    static int      FirstTime = 1;

    if(FirstTime)
    {
        FirstTime = 0;
        myYellowBrush = CreateSolidBrush(RGB(220,220,0));
        myGreenBrush  = CreateSolidBrush(RGB(0,200,0));
        myRedBrush    = CreateSolidBrush(RGB(200,0,0));
        myBlueBrush   = CreateSolidBrush(RGB(0,0,200));
    }

    hDC = BeginPaint( hWnd, &Paint );
    int saved = SaveDC(hDC);

    for(jj=0 ; jj<64 ; jj++)
    {
        result = Get_NodeStatus((unsigned char)jj, &UpDown, &IpAddr_nw);

        if(result == 0) continue;
        if(UpDown == 0) continue;

        if(IpAddr_nw == Get_MyIpAddr())
        {
            SelectObject(hDC,myYellowBrush);
            bMyNodeIsShown = TRUE;
        }
        else
        if(UpDown == 'U')
            SelectObject(hDC,myGreenBrush);
        else
        if(UpDown == 'N')
            SelectObject(hDC,myBlueBrush);
        else
            SelectObject(hDC,myRedBrush);

        Ellipse(hDC, X_POS_CIRCLE, Y_POS_CHECK + Ypos*DELTA,
                X_POS_CIRCLE+15, Y_POS_CHECK +Ypos*DELTA + 15);

        // Draw inner circels that indicates how I belive others see me
        // Note myself is allways 'NEW'
        unsigned __int64 BitMap64 = (unsigned __int64) 1 << jj;

        if(IpAddr_nw != Get_MyIpAddr())
        {
            if(UpDown == 'U')
            {
                if((G_pShm->BitMapToBePoolDistributed64 | G_pShm->BitMapBeingPoolDistributed64)
                    & BitMap64)
                {
                 SelectObject(hDC,myBlueBrush);
                 Ellipse(hDC, X_POS_CIRCLE,     Y_POS_CHECK + Ypos*DELTA,
                              X_POS_CIRCLE + 7, Y_POS_CHECK + Ypos*DELTA + 7);
                }
            }
            else
            if(UpDown == 'N')
            {
                if(((G_pShm->BitMapToBePoolDistributed64 | G_pShm->BitMapBeingPoolDistributed64)
                    & BitMap64) == 0)
                {
                    SelectObject(hDC,myGreenBrush);
                    Ellipse(hDC, X_POS_CIRCLE,     Y_POS_CHECK + Ypos*DELTA,
                                 X_POS_CIRCLE + 7, Y_POS_CHECK + Ypos*DELTA + 7);
                }
            }
        }

        Ypos++;
    }
    RestoreDC(hDC,saved);
    EndPaint( hWnd, &Paint );
}
/*--------------------- end Paint_it() -------------------*/

/********************************************************
* MUST handle insert new ??????????????
*********************************************************/
static void CreateNodeStatusControls( HWND hWnd )
{
    char       jj;
    int     result;
    DWORD    Ypos =0;
    char    *pTxt;
    char    buff[200];
    int     UpDown;
    DWORD   IpAddr_nw;
    static char NodeArr[64] = {-1};

    for(jj=0 ; jj<64 ; jj++)
    {
        if(NodeArr[jj] == -1) continue;

        DestroyWindow(GetDlgItem(hWnd,IDM_INDEX+NodeArr[jj]));
        DestroyWindow(GetDlgItem(hWnd,IDM_IPADDR+NodeArr[jj]));
        DestroyWindow(GetDlgItem(hWnd,IDM_STATUS+NodeArr[jj]));
        DestroyWindow(GetDlgItem(hWnd,IDM_RXCOUNT+NodeArr[jj]));
    }

    // this cases updating of edit boxes
    memset(g_SavedRxCount,0,sizeof(g_SavedRxCount));

    for(jj=0 ; jj<64 ; jj++)
    {
//      char buf[20];
        //sprintf(buf,"pShm=%X", pShm);

        result = Get_NodeStatus(jj, &UpDown, &IpAddr_nw);

        if(result == 0) continue;
        //if(pShm->NodeStatusTable[jj].Status == 0) continue;
        //if(Node_Status[jj].Exists) // delete it

        NodeArr[Ypos]   = jj;
        NodeArr[Ypos+1] = -1;

        // Index
        sprintf(buff,"%2d",jj);
        CreateWindow(  "static", buff,
                     WS_CHILD | WS_VISIBLE | WS_BORDER,
                    X_POS_INDEX, Y_POS_CHECK +Ypos*DELTA,WIDTH_INDEX,20,
                    hWnd,   (HMENU) (IDM_INDEX+jj), g_hInstance,NULL );

        // IpAddr
        pTxt = inet_ntoa(* (struct in_addr *) &IpAddr_nw);
        CreateWindow(  "edit", pTxt,
                     WS_CHILD | WS_VISIBLE | WS_BORDER, // |  SS_SUNKEN,
                    X_POS_IPADDR, Y_POS_CHECK +Ypos*DELTA,WIDTH_IPADDR,20,
                    hWnd,   (HMENU) (IDM_IPADDR+jj), g_hInstance,NULL );

        // Status
        if(IpAddr_nw == Get_MyIpAddr()) pTxt = "Me";
        else if(UpDown == 'U') pTxt = "Up";
        else if(UpDown == 'N') pTxt = "New";
        else pTxt = "Down";

        CreateWindow(  "edit", pTxt,
                  WS_CHILD| WS_VISIBLE | ES_READONLY | WS_BORDER,
                    X_POS_STATUS,
                    Y_POS_CHECK +Ypos*DELTA, WIDTH_STATUS,20,
                    hWnd,   (HMENU) (IDM_STATUS+jj), g_hInstance,NULL );

        // RxCount
        CreateWindow(  "edit", "-",
                   WS_CHILD| WS_VISIBLE | ES_READONLY | WS_BORDER,
                    X_POS_RXCOUNT,
                    Y_POS_CHECK +Ypos*DELTA, WIDTH_RXCOUNT,20,
                    hWnd,   (HMENU) (IDM_RXCOUNT+jj), g_hInstance,NULL );
        Ypos++;
    }
    g_NumNodes = Ypos;
}

/*******************************************************
*
*
********************************************************/
static void Resize_Window(HWND hWnd)
{
    RECT    rc;
    int     WinYsize;
    int     WinXsize;


    WinXsize = WIN_X2;
    WinYsize = 10 + 4 + (1+g_NumNodes) * DELTA + Y_POS_CHECK;

    GetWindowRect( hWnd, &rc );

    SetWindowPos(hWnd,HWND_TOPMOST, rc.left, rc.top,
            WinXsize,WinYsize,SWP_NOZORDER|SWP_NOACTIVATE);

    InvalidateRect(g_hWnd,NULL,FALSE);
    UpdateWindow(g_hWnd);
}

/*************************************
*
**************************************/
void Show_Node_Status(int Ix, char *pIpAddrText, char *pUpDownText)
{
    char buff[32];

    sprintf(buff,"%2d", Ix);

    SetDlgItemText(g_hWnd,IDM_INDEX  + Ix, buff);
    SetDlgItemText(g_hWnd,IDM_IPADDR + Ix, pIpAddrText);
    SetDlgItemText(g_hWnd,IDM_STATUS + Ix, pUpDownText);

    InvalidateRect(g_hWnd,NULL,FALSE);
    UpdateWindow(g_hWnd);
}


/**********************************************************
* Called at regular intervalls (polled)
*
* Update only for changes. This is to avoid stealing focus
* from info window.
************************************************************/
static void Update_NodeStatus(HWND hWnd)
{
    int     jj, result;
    DWORD   RxCount;
    char    *pIpAddrText;
    unsigned _int64 BitMask64;
    unsigned _int64 BitMapNew_New64  = (unsigned _int64) 0;
    unsigned _int64 BitMapUp_New64   = (unsigned _int64) 0;
    unsigned _int64 BitMapDown_New64 = (unsigned _int64) 0;
    unsigned _int64 SavedPdStatus64  = (unsigned _int64) 0;
    DWORD   ChangeCount = 0;
    int     UpDown;
    DWORD   IpAddr_nw;


    Get_BitMapNodesNew(0,  &BitMapNew_New64);
    Get_BitMapNodesUp(0,   &BitMapUp_New64);
    Get_BitMapNodesDown(0, &BitMapDown_New64);

    // First a quick check if any new nodes. If so redraw layout

    if(  (g_BitMap_New64 != BitMapNew_New64) || !bMyNodeIsShown)
    {
        g_BitMap_New64 = BitMapNew_New64;
        CreateNodeStatusControls(hWnd);
        ChangeCount++;
    }

    if(  (g_BitMap_Up64 != BitMapUp_New64) || !bMyNodeIsShown)
    {
        g_BitMap_Up64 = BitMapUp_New64;
        CreateNodeStatusControls(hWnd);
        ChangeCount++;
    }

    // Update RxCount edit boxes

    for(jj=0 ; jj<64 ; jj++)
    {
        result = Get_NodeStatus(jj, &UpDown, &IpAddr_nw);

        if(result == 0 )
        {
            SetDlgItemText(g_hWnd,IDM_RXCOUNT+jj, "???");
            continue;
        }
        else
        {
            RxCount = Get_RxCount(jj);
            if(RxCount != g_SavedRxCount[jj])
            {
                char buff[32];
                g_SavedRxCount[jj] = RxCount;
                sprintf(buff,"%3d", g_SavedRxCount[jj]);
                SetDlgItemText(g_hWnd,IDM_RXCOUNT+jj, buff);
            }
        }
    }

    // Scan for up/down changes - Set Bit Maps

    {
        //ChangeCount++;
        BitMask64 = (unsigned _int64) 1;
        for (jj=0 ; jj< 64 ; jj++)
        {
            result = Get_NodeStatus((unsigned char)jj, &UpDown, &IpAddr_nw);

            // New or down --> up
            if(((g_BitMap_Up64 & BitMask64) == (unsigned _int64) 0) // old down
                && (BitMapUp_New64 & BitMask64))  //new up
            {
                pIpAddrText = inet_ntoa( *(struct in_addr *) &IpAddr_nw);

                Show_Node_Status(jj, pIpAddrText, "Up");
            }
            else
            // up --> down
            if((g_BitMap_Up64 & BitMask64) // Up
                && ((BitMapUp_New64 & BitMask64)) == (unsigned _int64) 0) //new down
            {
                g_BitMapDown64 |= ((unsigned _int64)1<<jj);
                pIpAddrText = inet_ntoa( *(struct in_addr *) &IpAddr_nw);

                Show_Node_Status(jj, pIpAddrText, "Down");
            }
            BitMask64 = BitMask64<<1;
        }
        g_BitMap_Up64 = BitMapUp_New64;  // NY
    }

    if(ChangeCount)
        Resize_Window(hWnd);
/*---
    //else
    {
        if((G_pShm->BitMapToBePoolDistributed64 | G_pShm->BitMapBeingPoolDistributed64)
                    != SavedPdStatus64 )
        {
            SavedPdStatus64 = G_pShm->BitMapToBePoolDistributed64
                            | G_pShm->BitMapBeingPoolDistributed64;
            InvalidateRect(g_hWnd,NULL,FALSE);
            UpdateWindow(g_hWnd);
        }
    }
------*/
}

/***********************************************************
* Processes messages for the statistics window.
*
*************************************************************/
static int APIENTRY DoseMonWndProc( HWND hWnd, UINT message,
                                 UINT wParam, LONG lParam)
{
    BOOL bOk;
    unsigned __int64 SavedPdStatus64 = (unsigned __int64) 0;

    switch ( message )
    {
        case WM_CREATE:
            return FALSE;

        case WM_USER:
            InvalidateRect(g_hWnd,NULL,FALSE);
            break;

        case WM_USER+1:
            Update_NodeStatus(hWnd);

            //if((G_pShm->BitMapToBePoolDistributed64 | G_pShm->BitMapBeingPoolDistributed64)
            //          != SavedPdStatus64 )
            {
            //  SavedPdStatus64 = G_pShm->BitMapToBePoolDistributed64
            //                  | G_pShm->BitMapBeingPoolDistributed64;
                InvalidateRect(g_hWnd,NULL,FALSE);
                UpdateWindow(g_hWnd);
            }

            break;

        case WM_INITDIALOG:

            bOk = Load_DoseComDll();
            //if(bOk)
            Start_Log_Thread();
            CreateNodeStatusControls( hWnd );
            Resize_Window(hWnd);
            SetDlgItemText(hWnd,IDC_EDIT_MODE, "Mode: Local");

            // Change the icon for hwnd's window class.
            SetClassLong(hWnd, GCL_HICON,
                    (DWORD)LoadIcon(g_hInstance,MAKEINTRESOURCE(107)));
            break;

        case WM_COMMAND:
            switch ( LOWORD( wParam ) )
            {
                case IDC_BUTT_CANCEL:
                    SendMessage( hWnd, WM_CLOSE, 0, 0 );
                    return(TRUE);

                case IDC_BUTT_HELP:
                    MessageBox(NULL, "DoseMonitor ver 08-01-23", "Help", MB_OK);
                    break;

                case IDC_BUTT_INFO: //SIMCDHA:
                    Start_GetInfoDlg(g_hInstance, hWnd);
                    break;
            }
            break;

        case WM_SIZE:
            return FALSE;

        case WM_TIMER:
            //PostMessage(hWnd, WM_USER+1,WM_USER+1,WM_USER+1);
            //Update_NodeStatus(hWnd);
            //InvalidateRect(hWnd,NULL,FALSE);
            //SetTimer (hWnd, IDC_TIMER1, 1000, NULL);
            break;

        case WM_PAINT:
            Paint_it(hWnd);
            break;

        case WM_CLOSE:
            DestroyWindow(hWnd);
            EndDialog(hWnd, TRUE);
            return DefWindowProc( hWnd, message, wParam, lParam );

        case WM_DESTROY:  // trigged by WM_CLOSE
            g_IsRunning = FALSE;
            EndDialog(hWnd, TRUE);
            //PostQuitMessage(0); // this causes entire prog to terminate
            return FALSE;

        default:
            // Default behavior
            break;
            //return DefWindowProc( hWnd, message, wParam, lParam );
    }
    return FALSE;
}
/*--------------------- end LoginWndProc() ------------------*/

/*********************************************************************
* PURPOSE: Saves instance handle and creates main window
*
* COMMENTS:
* In this function, we save the instance handle in a global variable
* and create and display the main program window.
**********************************************************************/
BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
    g_hInstance = hInstance;

    g_hWnd = CreateDialog(hInstance,(LPTSTR) MAKEINTRESOURCE(IDD_DIALOG_MAIN),
                            NULL, (DLGPROC) DoseMonWndProc);

    ShowWindow(g_hWnd, nCmdShow);
    UpdateWindow(g_hWnd);

    if (!g_hWnd) return FALSE;
    return TRUE;
}

/*************************************************
*
**************************************************/
static void Timer_Thread(void *)
{
    Sleep(1000);

    for(;;)
    {
        Sleep(1000);
        PostMessage(g_hWnd, WM_USER+1,WM_USER+1,WM_USER+1);
    }
}

/********************************************************
* Brings up a dialog and ...
*
********************************************************/

int APIENTRY WinMain( HINSTANCE hInstance,
                        HINSTANCE, // hPrevInstance,
                        LPSTR, // lpCmdLine,
                        int) // nCmdShow )
{
    MSG     msg;


    InitInstance(hInstance, SW_SHOWNORMAL);
    g_IsRunning = TRUE;

    _beginthread( &Timer_Thread,0, NULL);

    // acquire and dispatch messages until a WM_QUIT message is received.
    while ( GetMessage( &msg, NULL, 0, 0 ) )
    {
        TranslateMessage( &msg );
        DispatchMessage( &msg );
        if(!g_IsRunning) break;
    }

    g_hWnd = NULL;
    return(0);
}
/*------------------------- end DoseMonitor.cpp -----------*/
