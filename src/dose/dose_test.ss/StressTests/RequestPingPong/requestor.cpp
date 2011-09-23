/******************************************************************************
*
* Copyright Saab AB, 2006-2011 (http://www.safirsdk.com)
*
* Created by: Mikael Wennerberg/ stmiwn
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

#include "requestor.h"
#include <DoseTest/SynchronousVolatileEntity.h>
#include <Safir/Dob/OverflowException.h>
#include <Safir/Dob/EntityIdResponse.h>
#include <iostream>

Requestor::Requestor(): m_create(true)
{

}

void Requestor::Start()
{
    m_Connection.Attach();
    std::wcout << "Requestor started" <<std::endl;


    DoseTest::SynchronousVolatileEntityPtr ent = DoseTest::SynchronousVolatileEntity::Create();
    ent->Info().SetVal(L"Nisse");

    m_Connection.CreateRequest(ent, Safir::Dob::Typesystem::HandlerId(), this);

}

void Requestor::OnResponse(const Safir::Dob::ResponseProxy responseProxy)
{
    if (responseProxy.IsSuccess())
    {

        if (Safir::Dob::Typesystem::Operations::IsOfType(responseProxy.GetTypeId(), Safir::Dob::EntityIdResponse::ClassTypeId))
        {
            Safir::Dob::EntityIdResponsePtr entityResponse = boost::static_pointer_cast<Safir::Dob::EntityIdResponse>(responseProxy.GetResponse());
            // Create response, send delete
            try
            {
                m_Connection.DeleteRequest(entityResponse->Assigned(), this);
            }
            catch (Safir::Dob::OverflowException)
            {
                m_Id = entityResponse->Assigned();
            }
        }
        else
        {
            // Delete response, send create
            DoseTest::SynchronousVolatileEntityPtr ent = DoseTest::SynchronousVolatileEntity::Create();
            ent->Info().SetVal(L"Nisse");
            try
            {
                m_Connection.CreateRequest(ent, Safir::Dob::Typesystem::HandlerId(), this);
            }
            catch (Safir::Dob::OverflowException)
            {
                m_Id = Safir::Dob::Typesystem::EntityId::EntityId();
            }
        }


        /*
        if (m_create)
        {
        m_Connection.DeleteRequest(Safir::Dob::Typesystem::EntityId(DoseTest::SynchronousVolatileEntity::ClassTypeId,Safir::Dob::Typesystem::InstanceId(666)), this);
        m_create = false;
        }
        else
        {
        DoseTest::SynchronousVolatileEntityPtr ent = DoseTest::SynchronousVolatileEntity::Create();
        ent->Info().SetVal(L"Nisse");
        m_Connection.CreateRequest(ent, Safir::Dob::Typesystem::InstanceId(666), Safir::Dob::Typesystem::HandlerId(), this);
        m_create = true;
        }
        */
    }
    else
    {
        std::wcout << "Requestor::OnResponse FAILED" <<std::endl;
        DoseTest::SynchronousVolatileEntityPtr ent = DoseTest::SynchronousVolatileEntity::Create();
        ent->Info().SetVal(L"Nisse");
        try
        {
            m_Connection.CreateRequest(ent, Safir::Dob::Typesystem::HandlerId(), this);
        }
        catch (Safir::Dob::OverflowException)
        {
            m_Id = Safir::Dob::Typesystem::EntityId::EntityId();
        }

    }

}

void Requestor::OnNotRequestOverflow()
{
    if (m_Id != Safir::Dob::Typesystem::EntityId::EntityId())
    {
        m_Connection.DeleteRequest(m_Id, this);
    }
    else
    {
        DoseTest::SynchronousVolatileEntityPtr ent = DoseTest::SynchronousVolatileEntity::Create();
        ent->Info().SetVal(L"Nisse");
        m_Connection.CreateRequest(ent, Safir::Dob::Typesystem::HandlerId(), this);
    }

}
