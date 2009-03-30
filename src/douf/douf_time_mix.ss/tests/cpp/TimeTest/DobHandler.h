/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
#pragma once
#include <Safir/Dob/Connection.h>
#include <Safir/Utilities/AceDispatcher.h>
#include <ace/Reactor.h>
#include "Consumer.h"

class DobHandler
{
public:
    DobHandler(const std::wstring appName);
    virtual ~DobHandler(void);

    virtual void Start() = 0;

protected:
    Consumer m_consumer;
    Safir::Dob::Connection m_Dob;
    Safir::Utilities::AceDispatcher     m_dispatcher;
    ACE_Reactor                         m_reactor; 
};

class SubscriptionHandler: public DobHandler
{
public:
    SubscriptionHandler(void);
    ~SubscriptionHandler(void);

protected:
    virtual void Start();
};

class RequestHandler: public DobHandler
{
public:
    RequestHandler(void);
    ~RequestHandler(void);

protected:
    virtual void Start();
};
