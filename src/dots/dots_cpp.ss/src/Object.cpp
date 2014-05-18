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
#include "Safir/Dob/Typesystem/BlobOperations.h"
#include "Safir/Dob/Typesystem/ContainerProxies.h"
#include <Safir/Utilities/DynamicLibraryLoader.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <iostream>

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

        void LoadLib(const std::string& path, const std::string& name)
        {
            std::string fullName = "dots_generated-" + name + "-cpp";

#if defined (_MSC_VER) && !defined (NDEBUG)
            fullName += 'd';
#endif
            
            try
            {
                Safir::Utilities::DynamicLibraryLoader loader;
                if (path.empty())
                {
                    loader.Load(fullName, false, true);
                }
                else
                {
                    loader.Load(fullName, path, false, true);
                }
            }
            catch (const std::logic_error& e)
            {
                SEND_SYSTEM_LOG (Critical,  << "Failed to load " << fullName.c_str() << " library: " << e.what());
                std::wostringstream ostr;
                ostr << "Failed to load " << fullName.c_str() << " library. Please check your configuration!";
                throw Safir::Dob::Typesystem::ConfigurationErrorException(ostr.str(), __WFILE__, __LINE__);

            }

        }

        bool LoadLibraries()
        {
            Safir::Utilities::Internal::ConfigReader reader;
            const boost::property_tree::ptree& ptree = reader.Typesystem();
            for (boost::property_tree::ptree::const_iterator it = ptree.begin();
                 it != ptree.end(); ++it)
            {
                const bool isSection = !it->second.empty();

                if (isSection)
                {
                    const std::string module = it->first;

                    std::string location;

                    const boost::optional<std::string> library_location = it->second.get_optional<std::string>("cpp_library_location");
                    if (library_location)
                    {
                        location = library_location.get();
                    }

                    const boost::optional<bool> dont_load = it->second.get_optional<bool>("dont_load");
                    if (!!dont_load && dont_load.get())
                    {
                        lllog(1) << "Not loading dots_generated module " << module.c_str() << " since dont_load is specified" << std::endl;
                    }
                    else
                    {
                        lllog(1) << "Loading dots generated module " << module.c_str() << std::endl;
                        LoadLib(location, module);
                    }
                }
            }

            return true;
        }

        const bool loadedLibraries = LoadLibraries();
        
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
