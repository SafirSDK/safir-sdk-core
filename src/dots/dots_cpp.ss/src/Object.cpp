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

#include "Safir/Dob/Typesystem/Exceptions.h"
#include "Safir/Dob/Typesystem/Object.h"
#include "Safir/Dob/Typesystem/ObjectFactory.h"
#include "Safir/Dob/Typesystem/ContainerProxies.h"
namespace Safir
{
namespace Dob
{
namespace Typesystem
{

#ifdef __GNUC__
    //obligatory static member initialization is needed by gcc
    const Safir::Dob::Typesystem::TypeId Object::ClassTypeId;
#endif

    //An anonymous namespace for object factory registration
    //this can never be called directly by anyone (since it is anonymous)
    namespace
    {
        ObjectPtr CreateObject(char const * const blob)
        {
            if (blob == NULL)
            {
                return ObjectPtr(new Object());
            }
            else
            {
                return ObjectPtr(new Object(blob));
            }
        }
        const bool registered =
            ObjectFactory::Instance().RegisterClass(Safir::Dob::Typesystem::Object::ClassTypeId,CreateObject);
    }

    //Check if anything in the object has change flags set
    bool Object::IsChanged()
    {
        return false;
    }

    //Recursively set all change flags in the object
    void
    Object::SetChanged(const bool /*changed*/)
    {

    }

    //
    // GetMember
    //
    ContainerBase &
    Object::GetMember(const Safir::Dob::Typesystem::MemberIndex /*member*/,
                      const Safir::Dob::Typesystem::ArrayIndex /*index*/)
    {
        throw SoftwareViolationException(L"Object does not have any members!", __WFILE__,__LINE__);
    }

    //
    // GetMember (const version)
    //
    const ContainerBase &
    Object::GetMember(const Safir::Dob::Typesystem::MemberIndex /*member*/,
                      const Safir::Dob::Typesystem::ArrayIndex /*index*/) const
    {
        throw SoftwareViolationException(L"Object does not have any members!", __WFILE__,__LINE__);
    }

    //
    // Clone
    //
    ObjectPtr
    Object::Clone() const
    {
        //must be done in two steps to avoid leaks (according to shared_ptr best practice)
        ObjectPtr p = ObjectPtr(new Object());
        return p;
    }

    //
    // Create
    //
    ObjectPtr
    Object::Create()
    {
        //must be done in two steps to avoid leaks (according to shared_ptr best practice)
        ObjectPtr p = ObjectPtr(new Object());
        return p;
    }

    //
    // CalculateBlobSize
    //
    Int32
    Object::CalculateBlobSize() const
    {
        static Int32 initialSize = -1;
        if (initialSize == -1)
        {
            initialSize = BlobOperations::GetInitialSize(ClassTypeId);
        }

        return initialSize;
    }

    //
    // WriteToBlob
    //
    void
    Object::WriteToBlob(char * /*blob*/, char * & /*beginningOfUnused*/) const
    {

    }

    //
    // Construct from blob
    //
    Object::Object(char const * const /*blob*/)
    {

    }
}
}
}
