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
#include "./Consumer.h"
#include <Safir/Test/TimeConversion.h>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <Safir/Time/TimeProvider.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Response.h>

Consumer::Consumer(void) 
{
}

Consumer::~Consumer(void)
{
}


void Consumer::OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
{
    if (entityProxy.GetTypeId() == Safir::Test::TimeConversion::ClassTypeId)
    {
        Safir::Test::TimeConversionPtr timeConv = boost::dynamic_pointer_cast<Safir::Test::TimeConversion>(entityProxy.GetEntity());

        if (timeConv->TimeStamp().IsNull())
        {
            std::cout<<" Safir.Test.TimeConversion.TimeStamp: NULL"<<std::endl;
        }
        else
        {
            //            std::cout << Safir::Time::TimeProvider::PtimeOf(*timeConv.TimeStamp().GetPtr()) << std::endl;
            std::cout << Safir::Time::TimeProvider::ToPtime(timeConv->TimeStamp().GetVal()) << std::endl;
        }
    }
}


void Consumer::OnDeletedEntity(const Safir::Dob::EntityProxy entityProxy, const bool deletedByOwner)
{
    std::wstring strTypeIdName;

    strTypeIdName = Safir::Dob::Typesystem::Operations::GetName( entityProxy.GetTypeId() );

    std::wcout<< strTypeIdName << L" Instance: " << entityProxy.GetInstanceId().ToString() <<
        L" REMOVED " << std::endl;

    std::cout<<std::endl;
}

void Consumer::OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId  typeId, const Safir::Dob::Typesystem::HandlerId& handlerId)
{

}

void Consumer::OnCreateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                               Safir::Dob::ResponseSenderPtr        responseSender)
{
    Safir::Dob::ResponsePtr rep;
    responseSender->Send(rep);
}

void Consumer::OnUpdateRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                               Safir::Dob::ResponseSenderPtr        responseSender)
{
    Safir::Dob::ResponsePtr rep;
    responseSender->Send(rep);
}

void Consumer::OnDeleteRequest(const Safir::Dob::EntityRequestProxy entityRequestProxy,
                               Safir::Dob::ResponseSenderPtr        responseSender)
{
    Safir::Dob::ResponsePtr rep;
    responseSender->Send(rep);
}
