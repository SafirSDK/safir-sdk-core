/******************************************************************************
*
* Copyright Saab AB, 2008 (http://www.safirsdk.com)
*
* Created by: Erik Adolfsson / sterad
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
#ifndef _DISPATCHING_STATES
#define _DISPATCHING_STATES

typedef enum
{
    Connected,
    Dispatched,
    Error
} States;


static std::wstring ToString(States s)
{
    switch (s)
    {
    case Connected: return L"Connected";
    case Dispatched: return L"Dispatched";
    case Error: return L"Error";
    default: return L"???";
    }
}

#endif
