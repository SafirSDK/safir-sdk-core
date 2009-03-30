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

typedef struct
{
    char    *pUrl;
//  char    *pCookie;
//  int     ContentLength;
    DWORD   PeerIpAddr;
} HTTP_RX_HDR_S;

class CTinyWebSrv
{

public:
    CTinyWebSrv(short Port,void *pCallBack);    // constructor
    ~CTinyWebSrv();                 // destructor

    void    SendMsg(SOCKET SockId, const char *format, ... );
    int     SendFile(SOCKET SockId, const char *pFileName);
    int     Send_HttpHeader(SOCKET SockId, DWORD UrlStatus,
                            const char *pExt);

private:
    static void __cdecl Server_Thread(void *lpdwParam);
    int     recvLine(SOCKET sock_id, char *pRxMsg);
    void    Send_ErrMsgPage(SOCKET SockId, int Status,
                     const char *pMsg1, const char *pMsg2,
                     const char *pMsg3, const char *pMsg4);
    void    Worker(SOCKET sock_id_client);
    short   m_HttpPort;
    int     SendText(SOCKET SockId, const char *pTxMsg, int size);

    SOCKET  m_SockId_http;

    //Application must provide the callback routine.
    void (* m_pWebCallBack)(SOCKET SockId, HTTP_RX_HDR_S *);

};

extern int Debug_Http;
/*----------------------------------- end TinyWebSrv.H -------*/
