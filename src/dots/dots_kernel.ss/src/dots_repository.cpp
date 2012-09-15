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

#include "dots_repository.h"
#include "dots_file_parser.h"
#include "dots_param.h"
#include "dots_error_handler.h"
#include "dots_allocation_helper.h"
#include <iostream>
#include <ace/Guard_T.h>
#include <ace/OS_NS_unistd.h>
#include <boost/bind.hpp>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    static const char* DOTS_SHMEM_NAME    = "DOB_TYPESYSTEM_DATA";

    //static instance initialization
    Repository * volatile Repository::m_instance = NULL;

    Repository & Repository::Instance()
    {
        if (m_instance == NULL)
        {
            static ACE_Thread_Mutex mtx;
            ACE_Guard<ACE_Thread_Mutex> lck(mtx);
            if (m_instance == NULL)
            {
                Init();
            }
        }
        return *m_instance;
    }

    void Repository::Init()
    {
        m_instance = new Repository();

        try
        {
            m_instance->m_startupSynchronizer.Start();
            return;
        }
        catch (const boost::interprocess::bad_alloc&)
        {
            lllerr << "Ran out of shared memory while loading types and parameters." <<std::endl
                   << "Please adjust the parameter Safir.Dob.NodeParameters.TypesystemSharedMemorySize" << std::endl;
        }
        catch (const std::exception & exc)
        {
            lllerr << "Loading of dots_kernel failed with exception description: " << exc.what() << std::endl;
        }
        catch (...)
        {
            lllerr << "Loading of dots_kernel failed with ... exception." << std::endl;
        }
        exit(0);
    }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4355)
    //disable using this in constructor warning
#endif

    Repository::Repository():
        m_startupSynchronizer("DOTS_INITIALIZATION",this)
    {

    }

#ifdef _MSC_VER
#pragma warning(pop)
#endif

    Repository::~Repository()
    {

    }

    void Repository::Create()
    {
        LoadData();
    }

    void Repository::Use()
    {
        m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                             (boost::interprocess::open_only,
                              DOTS_SHMEM_NAME));
        m_classDb.reset(new ClassDatabase(open_only,*m_sharedMemory));
        m_parameterDb.reset(new ParameterDatabase(open_only,*m_sharedMemory));
        m_propertyDb.reset(new PropertyDatabase(open_only,*m_sharedMemory));
        m_enumDb.reset(new EnumDatabase(open_only,*m_sharedMemory));
        m_exceptionDb.reset(new ExceptionDatabase(open_only,*m_sharedMemory));
    }

    void Repository::Destroy()
    {
        boost::interprocess::shared_memory_object::remove(DOTS_SHMEM_NAME);
    }


    size_t GetSharedMemorySize(const DobClasses & classes)
    {
        for (DobClasses::const_iterator classIt = classes.begin();
             classIt != classes.end(); ++classIt)
        {
            if (classIt->m_name == "Safir.Dob.NodeParameters")
            {
                const DobParameters & parameters = classIt->m_parameters;
                for (DobParameters::const_iterator paramIt = parameters.begin();
                     paramIt != parameters.end(); ++paramIt)
                {
                    if (paramIt->m_name== "TypesystemSharedMemorySize")
                    {
                        ENSURE(paramIt->m_type == Int32MemberType, << "Safir.Dob.NodeParameters.TypesystemSharedMemorySize must be of type Int32");
                        ENSURE(paramIt->m_values.size() == 1, << "Safir.Dob.NodeParameters.TypesystemSharedMemorySize must not be an array");
                        ENSURE(paramIt->m_values[0].m_isNull == false, << "Safir.Dob.NodeParameters.TypesystemSharedMemorySize must not be null");
                        ENSURE(paramIt->m_values[0].m_valueFromParameter == false, << "Safir.Dob.NodeParameters.TypesystemSharedMemorySize must not be a valueRef");
                        const std::string strValue = paramIt->m_values[0].m_value;
                        ENSURE(IsInt(strValue.c_str()), << "Safir.Dob.NodeParameters.TypesystemSharedMemorySize must be a valid integer. Current value '" << strValue << "'");
                        return boost::lexical_cast<Int32>(strValue) * 1024 * 1024;
                    }
                }
            }
        }
        throw InternalException("Failed to find parameter Safir.Dob.NodeParameters.TypesystemSharedMemorySize", __FILE__,__LINE__);
    }

    void Repository::LoadData()
    {
        FileParser fp;
        if (!fp.ParseFiles())
        {
            std::wcout << "Failed to parse files. Errors should be indicated above." << std::endl;
            exit(-1);
        }

        boost::interprocess::shared_memory_object::remove(DOTS_SHMEM_NAME);
        m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                             (boost::interprocess::create_only,
                              DOTS_SHMEM_NAME,
                              GetSharedMemorySize(fp.ResultClasses())));

        AllocationHelper allocHelper(m_sharedMemory.get());


        //Create class database in shared memory
        m_classDb.reset(new ClassDatabase(create_and_initialize, allocHelper));

        //Create property database in shared memory
        m_propertyDb.reset(new PropertyDatabase(create_and_initialize, allocHelper));

        //Create enum database in shared memory
        m_enumDb.reset(new EnumDatabase(create_and_initialize, allocHelper));

        //Create parameter database in shared memory
        m_parameterDb.reset(new ParameterDatabase(create_and_initialize, fp.ParameterSize(), allocHelper));

        //Create exception database in shared memory
        m_exceptionDb.reset(new ExceptionDatabase(create_and_initialize, allocHelper));

        //Store enum types in shared memory
        std::for_each(fp.ResultEnums().begin(),fp.ResultEnums().end(),
                      boost::bind(&EnumDatabase::Insert,m_enumDb.get(),_1,boost::ref(allocHelper)));

        //store exceptions in shared memory
        m_exceptionDb->InsertExceptions(fp.ResultExceptions(), allocHelper);

        //Store classes in shared memory
        m_classDb->InsertClasses(fp.ResultClasses(), *m_enumDb, allocHelper);

        //Store parameters in shared memory
        std::for_each(fp.ResultClasses().begin(),fp.ResultClasses().end(),
                      boost::bind(&Repository::InsertParameter,this,_1,boost::ref(allocHelper)));

        int tries = 0;
        while (tries < 20 && !m_retryParameters.empty())
        {
            ++tries;
            InsertRetryParameters(allocHelper);
        }

        //Store properties in shared memory
        std::for_each(fp.ResultProperties().begin(),fp.ResultProperties().end(),
                      boost::bind(&PropertyDatabase::Insert,m_propertyDb.get(),_1,boost::cref(*m_classDb),boost::cref(*m_enumDb),boost::ref(allocHelper)));


        //Update classes with property mappings when classes and properties already exists in shared memory
        std::for_each(fp.ResultClasses().begin(),fp.ResultClasses().end(),
                      boost::bind(&Repository::InsertPropertyMapping,this,_1,boost::ref(allocHelper)));
    }


    void Repository::InsertPropertyMapping(const DobClass & tmpClass, AllocationHelper & allocHelper)
    {
        for (DobPropertyMappings::const_iterator propMapIt = tmpClass.m_propertyMappings.begin();
             propMapIt != tmpClass.m_propertyMappings.end(); ++propMapIt)
        {
            ClassDescription * cde = m_classDb->FindClass(propMapIt->m_classTypeId);
            const PropertyDescription * pde= Properties().FindProperty(propMapIt->m_propertyTypeId);
            if (cde==NULL)
            {
                std::string info="Class "+propMapIt->m_className+" does not exist!";
                ErrorHandler::Error("Property Mapping Error", info, "dots_repository");
                return;
            }
            else if (pde==NULL)
            {
                std::string info="Property "+propMapIt->m_propertyName+" does not exist!";
                ErrorHandler::Error("Property Mapping Error", info, "dots_repository");
                return;
            }


            PropertyMappingDescription pmd(static_cast<Size>(propMapIt->m_mappings.size()),
                                           pde,
                                           allocHelper);

            for (Temporary::MappingMembers::const_iterator memMapIt = propMapIt->m_mappings.begin();
                 memMapIt != propMapIt->m_mappings.end(); ++memMapIt)
            {
                //lllout << "  mapping " << std::distance(propMapIt->m_mappings.begin(),memMapIt) << std::endl;
                switch(memMapIt->m_kind)
                {
                case Temporary::NullMapping:
                    {
                        pmd.AddMapping(MemberMapping());
                    }
                    break;
                case Temporary::ClassMemberReferenceMapping:
                    {
                        MemberMapping & mapping = pmd.AddMapping(MemberMapping(static_cast<Size>(memMapIt->m_binaryClassMemberReference.size()),allocHelper));

                        for (Temporary::BinaryClassMemberReference::const_iterator refElIt = memMapIt->m_binaryClassMemberReference.begin();
                             refElIt != memMapIt->m_binaryClassMemberReference.end();++refElIt)
                        {
                            mapping.AddMemberReferenceLevel(refElIt->m_classMember,refElIt->m_index);
                        }
                    }
                    break;

                case Temporary::ParameterMapping:
                    {
                        bool hasRealParameterMappings = false;
                        for (size_t valueIndex=0; valueIndex<memMapIt->m_parameter.m_values.size(); valueIndex++)
                        {
                            if (memMapIt->m_parameter.m_values[valueIndex].m_valueFromParameter)
                            {
                                hasRealParameterMappings=true;
                            }
                        }

                        if (hasRealParameterMappings)
                        {
                            std::string info="Too advanced syntax for this version of DOTS! Can't resolve parameter "+memMapIt->m_parameter.m_name;
                            ErrorHandler::Error("Property Mapping Error",
                                                info,
                                                propMapIt->m_filename,
                                                memMapIt->m_parameter.m_values[0].m_lineNumber,
                                                "dots_repository");
                            exit(-1);
                        }

                        ParameterDescription pd(memMapIt->m_parameter.m_name,
                                                memMapIt->m_parameter.m_type,
                                                static_cast<Size>(memMapIt->m_parameter.m_values.size()),
                                                m_parameterDb->Insert(memMapIt->m_parameter,
                                                                      *m_enumDb,
                                                                      allocHelper),
                                                allocHelper);

                        pmd.AddMapping(MemberMapping(pd,allocHelper));
                    }
                    break;
                }
                if (static_cast<Size>(memMapIt->m_propertyMemberIndex) + 1 != pmd.NumberOfMappings())
                {
                    throw "Index problem in InsertPropertyMapping, a mapping had a bad index!";
                }
            }
            cde->AddPropertyMapping(pmd);
        }
    }



    void Repository::InsertParameter(const DobClass & tmpClass, AllocationHelper & allocHelper)
    {
        lllout << "Adding Parameters to class " << tmpClass.m_name.c_str() << std::endl;

        ClassDescription & classDesc = *m_classDb->FindClass(tmpClass.m_typeId);

        for (DobParameters::const_iterator it = tmpClass.m_parameters.begin();
             it != tmpClass.m_parameters.end(); ++it)
        {
            lllout << "  adding parameter " << it->m_name.c_str() << std::endl;
            try
            {
                classDesc.AddParameter(ParameterDescription(it->m_name,
                                                            it->m_type,
                                                            static_cast<Size>(it->m_values.size()),
                                                            m_parameterDb->Insert(*it, *m_enumDb, allocHelper),
                                                            allocHelper));
            }
            catch (const DeserializationFailure &)
            {
                lllout << "Failed to add parameter " << it->m_name.c_str() << " to class " << tmpClass.m_name.c_str() << std::endl;
                m_retryParameters.push_back(std::make_pair(tmpClass.m_typeId,*it));
            }
        }
    }

    void Repository::InsertRetryParameters(AllocationHelper & allocHelper)
    {
        for (RetryParameterTable::iterator it = m_retryParameters.begin();
             it != m_retryParameters.end();)
        {
            ClassDescription & classDesc = *m_classDb->FindClass(it->first);

            try
            {
                classDesc.AddParameter(ParameterDescription(it->second.m_name,
                                                            it->second.m_type,
                                                            static_cast<Size>(it->second.m_values.size()),
                                                            m_parameterDb->Insert(it->second, *m_enumDb, allocHelper),
                                                            allocHelper));
                lllout << "Retry add was successful for parameter " << it->second.m_name.c_str() << " to class " << classDesc.Name() << std::endl;
                it = m_retryParameters.erase(it);
            }
            catch (const DeserializationFailure &)
            {
                lllout << "Failed to retry add parameter " << it->second.m_name.c_str() << " to class " << classDesc.Name() << std::endl;
                ++it;
            }
        }
    }

}
}
}
}
