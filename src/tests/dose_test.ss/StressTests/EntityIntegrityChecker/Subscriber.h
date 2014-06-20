/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
*
* Created by: Lars Hagström / stlrha
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

#ifndef __SUBSCRIBER_H__
#define __SUBSCRIBER_H__

#include <Safir/Dob/Connection.h>

class Subscriber :
    public Safir::Dob::EntitySubscriber
{
public:
    Subscriber();

private:
    virtual void OnNewEntity(const Safir::Dob::EntityProxy entityProxy)
    {HandleEntity(entityProxy);}
    virtual void OnUpdatedEntity(const Safir::Dob::EntityProxy entityProxy)
    {HandleEntity(entityProxy);}
    virtual void OnDeletedEntity(const Safir::Dob::EntityProxy /*entityProxy*/,
                                 const bool                    /*deletedByOwner*/) {}

    void HandleEntity(const Safir::Dob::EntityProxy& entityProxy);

    Safir::Dob::SecondaryConnection m_connection;

    int m_numSubscriptions;
};

#endif

