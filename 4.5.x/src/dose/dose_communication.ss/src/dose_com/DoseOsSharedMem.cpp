/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

/*****************************************************************
* DoseOsSharedMem.cpp - a part of DoseComDll - For LINUX and WIN32
*
* Create Shared momory
*
* Entry: void *DoseOs::Shared_Memory_Create(int Size)
*******************************************************************/

#define IS_USING_SOCKETS
#define IS_USING_THREADS
#include "DosePlatform.h"
#include "PrintError.h"

#include "DoseOsInterface.h"

extern volatile int * volatile pDbg;

//####################
// Section WIN32
//####################

#ifdef _WIN32

#define SHM_NAME L"DoseComSharedMem"

//================================================

static volatile char *g_pShm = NULL;

/*****************************************************************
* Create and int shared memory.
* Returns: NULL if any error. A handle if OK or it already exists
*
* *pRetVal = ERROR_ALREADY_EXISTS if it already exists, this means
*            we entered routine before the one before us was finished.
*          = 0 if OK
*          = -1 if some error
******************************************************************/

#define STARTUP_FLAG 0x13245768

static HANDLE create_shm( const wchar_t *pName, int Size,
                         int *pRetVal, char **ppRetShm)
{
    HANDLE  hHandle = NULL;
    DWORD   shared_mem_size = Size;
    SECURITY_ATTRIBUTES sa;
    SECURITY_DESCRIPTOR sd;


    *ppRetShm = NULL; //default if error

    //-------------------------------------------------------------
    // When handle parameter == 0xFFFFFFFF tells Windows NT that
    // you are not mapping on disk.
    //------------------------------------------------------------

    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;
    sa.lpSecurityDescriptor = &sd;

    if(!InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION))
    {
        PrintErr(GetLastError(),
            "create_shm() InitializeSecurityDescriptor failed\n");
        return(NULL);
    }
    if(!SetSecurityDescriptorDacl(&sd, TRUE, (PACL)NULL, FALSE))
    {
        PrintErr(GetLastError(),
            "create_shm() SetSecurityDescriptorDacl failed\n");
        return(NULL);
    }

    //-------------------------------------------------------------------
    // If the function succeeds, the return value is a handle to the
    // file-mapping object. If the object existed before the function call,
    // the GetLastError function returns ERROR_ALREADY_EXISTS, and the
    // return value is a valid handle to the existing file-mapping object
    // (with its current size, not the new specified size. If the mapping
    // object did not exist, GetLastError returns zero.
    // If the function fails, the return value is NULL. To get extended
    // error information, call GetLastError.
    //--------------------------------------------------------------------

    hHandle = CreateFileMapping(INVALID_HANDLE_VALUE, // from above
                                &sa,                //&security_attrib
                                PAGE_READWRITE,
                                0, shared_mem_size, //high/low 32 bits of fsize
                                pName);
    if (hHandle == NULL)
    {
        PrintErr(GetLastError(), "create_shm() CreateFileMapping failed E=%d\n");
        return(NULL);
    }

    // In this case it already exists the handle is valid
    if ( GetLastError() == ERROR_ALREADY_EXISTS)
    {
        *pRetVal = ERROR_ALREADY_EXISTS;
        return(hHandle);
    }

    *pRetVal = 0; // OK
    *ppRetShm = 0; //pShm;

    return(hHandle);
}
/*----------------------- end create_shm() ----------------*/

/************************************************************
* Connect to shared memory. Store pointer to it in Shm_p.
*
* Returns:
* -1 if some error
*  1 if this was the first user
*  0 if not the first user
************************************************************/

void *DoseOs::Shared_Memory_Create(int Size)
{
    HANDLE  hHandle = NULL;
    int     RetVal;
    volatile char   *pShm = NULL;
    int     bAlreadyExists = FALSE;


    if (g_pShm != NULL)   // already done by this process
    {
        return((char *) g_pShm);
    }

    if(*pDbg) PrintDbg("DoseOs::Shared_Memory_Create()\n");

    //------------------------------------------------------------
    // When handle parameter == 0xFFFFFFFF tells Windows NT that
    // you are not mapping on disk.
    //------------------------------------------------------------

    hHandle = OpenFileMapping(FILE_MAP_READ | FILE_MAP_WRITE,
                                 FALSE, SHM_NAME);

    if (hHandle != NULL)  // Does not exist, create it
    {
        //p.rintf("Shm already exist\n");
        bAlreadyExists = TRUE;
    }
    else
    {
        // Try create it
        hHandle = create_shm(SHM_NAME, Size, &RetVal, (char **) &pShm);

        if(RetVal == ERROR_ALREADY_EXISTS) bAlreadyExists = TRUE;

        //-------------------------------------------------------------------
        //Cases:
        //1. shm_handle ==0 ==> some error. give up
        //2. shm_handle !=0, RetVal==0 ==> A new is created all is done.
        //3. shm_handle !=0, RetVal==ERROR_ALREADY_EXISTS ==>OK and more to do
        //--------------------------------------------------------------------
        if ( hHandle == NULL)
        {
            PrintErr(GetLastError(), "Connect_To_Shm() create_shm failed\n");
            return(NULL);
        }
    }

    /*----------------------------------------------------
    * File mapping created successfully.
    * Map a view of the file into the address space.
    *----------------------------------------------------*/

    pShm = (char *) MapViewOfFile(hHandle,
                            FILE_MAP_WRITE | FILE_MAP_READ,
                            0,0,0); // starting byte in file
    if (pShm == NULL)
    {
        PrintErr(GetLastError(), "Connect_To_Shm() MapViewFile() failed\n");
        return(NULL);
    }

    if(!bAlreadyExists)
    {
        memset( (void *) pShm, 0, Size);
    }
    //------------------------------------------------------
    // Suppose we interrupted the caller to create_shm()
    // before it completed. In that case we must wait until
    // completion. We use the last variable initialized by
    // create_shm() to test that.
    //------------------------------------------------------

    g_pShm = pShm;

    return((char *) g_pShm); // OK
}
/*----------------------- end Connect_To_Shm() ----------------*/

#endif  // _WIN32

//####################
// Section LINUX
//####################

#ifdef _LINUX
/*******************************************************
*
********************************************************/

void *DoseOs::Shared_Memory_Create(int Size)
{
    key_t   key = 0x1C2E3A4B;  // ????? a better value ????
    void    *pShm;
    int shmid;

    // The shmget() function shall return the shared memory identifier
    // associated with key.
    // int shmget(key_t key, size_t size, int shmflg);
    shmid = shmget(key, Size, 0666);

    if (shmid == -1)
    {
        if (ENOENT != errno) // ENOENT ==> does not exist, try create
        {
            PrintErr(errno, "Shared_Memory_Create() shmget() failed\n");
            return (NULL);
        }

        // Try to create
        shmid = shmget(key, Size, IPC_CREAT | 0666);

        if (shmid == -1)
        {
            PrintErr(errno, "Shared_Memory_Create() shmget(IPC_CREATE) failed.\n");
            return(NULL);
        }

        if (*pDbg >= 1)
            PrintDbg("Shared_Memory_Create() Created shared memory\n");
    }

    // The shmat() function attaches the shared memory segment associated
    // with the shared memory identifier specified by shmid to the address
    // space of the calling process.
    pShm = shmat(shmid, NULL, 0);

    if (pShm == (void*)-1)
    {
        PrintErr(errno, "Shared_Memory_Create() shmat() failed\n");
        return(NULL);
    }

    return(pShm); //OK
}
#endif // _LINUX

/*------------- end DoseOsSharedMem.cpp ---------------------*/
