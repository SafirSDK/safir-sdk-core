/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safir.sourceforge.net)
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


#include <Safir/Dob/Consumer.h>
#include <Safir/Dob/Connection.h>


class Requestor:  public Safir::Dob::Requestor
{

public:
    Requestor();
    void Start();

protected:
    virtual void OnResponse(const Safir::Dob::ResponseProxy responseProxy);
    virtual void OnNotRequestOverflow();


private:
    bool m_create;
    Safir::Dob::Typesystem::EntityId m_Id;
    Safir::Dob::SecondaryConnection m_Connection;
   
};
