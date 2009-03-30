/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_repository_h
#define _dots_repository_h

#include "dots_internal_defs.h"
#include "dots_temporary_descriptions.h"
#include "dots_class_database.h"
#include "dots_parameter_database.h"
#include "dots_property_database.h"
#include "dots_enum_database.h"
#include <boost/scoped_ptr.hpp>
#include <list>
#include <Safir/Utilities/StartupSynchronizer.h>
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * The main database class in DOTS. Constains all classes, parameters and properies.
     */
    class Repository:
        private boost::noncopyable,
        public Safir::Utilities::Synchronized
    {
    public:

        static Repository & Instance();

        static const ParameterDatabase &  Parameters()  {return *Instance().m_parameterDb;}
        static const EnumDatabase &       Enums()       {return *Instance().m_enumDb;}
        static const PropertyDatabase &   Properties()  {return *Instance().m_propertyDb;}
        static const ClassDatabase &      Classes()     {return *Instance().m_classDb;}

    private:
        //initilisation of the databases
        //If shmem is not created this will create and initialize the memory, otherwise it will just open it.
        static void Init();


        //StartupSynchronizer stuff
        virtual void Create();
        virtual void Use();
        virtual void Destroy();

        Repository();
        ~Repository();

        void LoadData();


        boost::scoped_ptr<boost::interprocess::managed_shared_memory> m_sharedMemory;
        boost::scoped_ptr<EnumDatabase> m_enumDb;
        boost::scoped_ptr<ParameterDatabase> m_parameterDb;
        boost::scoped_ptr<ClassDatabase> m_classDb;
        boost::scoped_ptr<PropertyDatabase> m_propertyDb;


        static Repository * volatile m_instance;

        //Functions for inserting stuff.
        //other insertion functions are in the respective database classes, but these were
        //easiest to put here.
        void InsertParameter(const DobClass & tmpClass, AllocationHelper & allocHelper);
        void InsertPropertyMapping(const DobClass & tmpClass, AllocationHelper & allocHelper);


        //parameters that have to be tried again:
        typedef std::list<std::pair<TypeId,DobParameter> > RetryParameterTable;
        RetryParameterTable m_retryParameters;
        void InsertRetryParameters(AllocationHelper & allocHelper);

        Safir::Utilities::StartupSynchronizer m_startupSynchronizer;
    };
}
}
}
}
#endif

