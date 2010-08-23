/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
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

#ifndef _DoseCom_Interface_Classes_h
#define _DoseCom_Interface_Classes_h

//----------------------------------------------------------------------
// This is a base class that is used for buffer handling.
// DoseMain creates a class that is derived from this class.
// The Allocate() and Deallocate() routines are implemented by DoseMain.
// DoseCom does not know and does not care how these are implemented.
// A pointer to this class is passed in routine int DoseCom_Init()
// where it is saved in a variable in DoseCom.
// DoseCom uses these to get and free buffers.
//---------------------------------------------------------------------

class DoseComAllocator
{
public:
  //abstract class needs virtual destructor
  virtual ~DoseComAllocator() {};

  // returns NULL if it is not possible to allocate memory.
  virtual char * Allocate(const size_t size) = 0;

  // Undefined result if called with a buffer that was not successfully
  // allocated with Allocate although it is ok to call it with pBuf = NULL
  virtual void Deallocate(char *pBuf) = 0;
};

//----------------------------------------------------------------------
// This is a base class that is used for signaling Events
// DoseMain creates a class that is derived from this class.
// The are implemented by DoseMain.
// DoseCom does not know and does not care how these are implemented.
// A pointer to this class is passed in routine int DoseCom_Init()
// where it is saved in a variable in dose_com.
// dose_com uses these to send notifications of certain events.
//---------------------------------------------------------------------

class DoseComNotificationHandler
{
public:
    virtual ~DoseComNotificationHandler() {}

    // these can be called from any thread, and must be implemented to be non-blocking.
    virtual void NotifyIncomingData(const int priorityChannel) = 0;
    virtual void NotifyQueueNotFull(const int priorityChannel) = 0;
    virtual void NotifyNodeStatusChanged() = 0;
    virtual void NotifyStartPoolDistribution() = 0;
    virtual void NotifyRequestPoolDistribution(const int nodeId) = 0;
};

#endif //_DoseCom_Interface_Classes_h
