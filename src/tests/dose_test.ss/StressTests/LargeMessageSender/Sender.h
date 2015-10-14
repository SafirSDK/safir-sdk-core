/******************************************************************************
*
* Copyright Saab AB, 2006-2015 (http://safirsdkcore.com)
*
* Created by: Samuel Waxin / samuel.waxin@consoden.se
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

#ifndef __SENDER_H__
#define __SENDER_H__

#include <Safir/Dob/Connection.h>

#include <DoseTest/ComplexGlobalMessage.h>
#include <boost/function.hpp>

class Sender :
    public Safir::Dob::MessageSender
{
public:
    Sender(int sendCount, int timeOut, const boost::function<void(void)> & finishedCallback);

    void Start();
    void SendSome();

protected:
    virtual void OnNotMessageOverflow();

    Safir::Dob::SecondaryConnection m_connection;

    DoseTest::ComplexGlobalMessagePtr m_message;

    int m_sendCount;
    int m_timeOut;
    int m_noSentMessages;

    const boost::function<void(void)> m_finishCallback;
};

#endif

