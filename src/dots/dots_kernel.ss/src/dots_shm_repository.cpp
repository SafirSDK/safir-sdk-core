/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
*
* Created by: Joel Ottosson / joot
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
#include <iostream>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include <Safir/Utilities/Internal/SystemLog.h>
#include "dots_shm_repository.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    static const char* DOTS_SHMEM_NAME = "DOB_TYPESYSTEM_DATA";

    RepositoryKeeper& RepositoryKeeper::Instance()
    {
        static RepositoryKeeper instance;
        return instance;
    }

    void RepositoryKeeper::Initialize(size_t sharedMemorySize, const std::vector<boost::filesystem::path>& paths)
    {
        try
        {
            RepositoryKeeper* instance=&RepositoryKeeper::Instance();
            instance->m_sharedMemorySize=sharedMemorySize;
            instance->m_paths.insert(instance->m_paths.begin(), paths.begin(), paths.end());
            instance->m_startupSynchronizer.Start(instance);
            if (instance->m_repository!=NULL)
            {
                instance->m_blobLayout.reset(new Safir::Dob::Typesystem::Internal::BlobLayout<RepositoryShm>(instance->m_repository));
                return; //loaded ok
            }
        }
        catch (const std::exception & exc)
        {
            SEND_SYSTEM_LOG(Error, << "Loading of dots_kernel failed with exception description: " << exc.what());
        }
        catch (...)
        {
            SEND_SYSTEM_LOG(Error,  << "Loading of dots_kernel failed with ... exception.");
        }
        exit(1);
    }

    bool RepositoryKeeper::RepositoryCreatedByThisProcess()
    {
        return Instance().m_repositoryCreatedByThisProcess;
    }

    const RepositoryShm* RepositoryKeeper::GetRepository()
    {
        return Instance().m_repository;
    }

    const Safir::Dob::Typesystem::Internal::BlobLayout<RepositoryShm>* RepositoryKeeper::GetBlobLayout()
    {
        return Instance().m_blobLayout.get();
    }

    RepositoryKeeper::RepositoryKeeper()
        :m_startupSynchronizer("DOTS_INITIALIZATION")
        ,m_sharedMemorySize(0)
        ,m_paths()
        ,m_repositoryCreatedByThisProcess(false)
    {
    }

    RepositoryKeeper::~RepositoryKeeper()
    {
    }

    void RepositoryKeeper::Create()
    {
        LoadData();
        m_repositoryCreatedByThisProcess=true;
    }

    void RepositoryKeeper::Use()
    {
        try
        {
            m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                                 (boost::interprocess::open_read_only,
                                  DOTS_SHMEM_NAME));
        }
        catch (...)
        {
            m_repository=NULL;
            return;
        }

        m_repository=m_sharedMemory->find<RepositoryShm>("DOTS_REPOSITORY").first;
    }

    void RepositoryKeeper::Destroy()
    {
        boost::interprocess::shared_memory_object::remove(DOTS_SHMEM_NAME);
    }

    void RepositoryKeeper::LoadData()
    {
        //-------------------------------------------------
        //Parse dou and dom files into local repository
        //-------------------------------------------------
        boost::shared_ptr<const Safir::Dob::Typesystem::Internal::TypeRepository> localRepository;
        try
        {
            localRepository=Safir::Dob::Typesystem::Internal::ParseTypeDefinitions(m_paths);
        }
        catch(const Safir::Dob::Typesystem::Internal::ParseError& err)
        {
            std::wcout<<"********** Parse Error **********************************************"<<std::endl;
            std::wcout<<"* Label: "<<err.Label().c_str()<<std::endl;
            std::wcout<<"* Descr: "<<err.Description().c_str()<<std::endl;
            std::wcout<<"* File:  "<<err.File().c_str()<<std::endl;
            std::wcout<<"* ErrId: "<<err.ErrorId()<<std::endl;
            std::wcout<<"*********************************************************************"<<std::endl;
            localRepository.reset();
            m_repository=NULL;
            return;
        }

        try
        {
            //-------------------------------------------------
            //Copy localRepository into shared memory
            //-------------------------------------------------
            boost::interprocess::shared_memory_object::remove(DOTS_SHMEM_NAME);
            m_sharedMemory.reset(new boost::interprocess::managed_shared_memory
                                 (boost::interprocess::create_only,
                                  DOTS_SHMEM_NAME,
                                  m_sharedMemorySize));
            m_repository=m_sharedMemory->construct<RepositoryShm>("DOTS_REPOSITORY")(m_sharedMemory.get());

            //----------------------------------
            //Enums
            //----------------------------------
            std::set<DotsC_TypeId> typeIdVec;
            localRepository->GetAllEnumTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                m_repository->m_enums.insert(EnumMapShm::value_type(*it, EnumDescriptionShm(localRepository->GetEnum(*it), m_sharedMemory.get())));
            }

            //----------------------------------
            //Exceptions
            //----------------------------------
            typeIdVec.clear();
            localRepository->GetAllExceptionTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                m_repository->m_exceptions.insert(ExceptionMapShm::value_type(*it, ExceptionDescriptionShm(localRepository->GetException(*it), m_sharedMemory.get())));
            }
            for (ExceptionMapShm::iterator it=m_repository->m_exceptions.begin(); it!=m_repository->m_exceptions.end(); ++it)
            {
                const ExceptionDescription* base=localRepository->GetException(it->first)->GetBaseClass();
                if (base)
                {
                    it->second.SetBaseClass(&(m_repository->m_exceptions.find(base->GetTypeId())->second));
                    //
                }
            }

            //----------------------------------
            //Properties
            //----------------------------------
            typeIdVec.clear();
            localRepository->GetAllPropertyTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                m_repository->m_properties.insert(PropertyMapShm::value_type(*it, PropertyDescriptionShm(localRepository->GetProperty(*it), m_sharedMemory.get())));
            }

            //--------------------------------------------
            //Classes, parameters and propertyMappings
            //--------------------------------------------
            typeIdVec.clear();
            localRepository->GetAllClassTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                //class
                const ClassDescription* cd=localRepository->GetClass(*it);
                ClassDescriptionShm* cdShm= &(m_repository->m_classes.insert(ClassMapShm::value_type(*it, ClassDescriptionShm(cd, m_sharedMemory.get()))).first->second);

                //parameters
                int numParams=cd->GetNumberOfParameters();
                int startParam=numParams-cd->GetNumberOfOwnParameters();
                for (int i=startParam; i<numParams; ++i)
                {
                    const ParameterDescription* paramDesc=cd->GetParameter(i);
                    ParameterDescriptionShmPtr paramPtr=&(m_repository->m_params.insert(ParameterMapShm::value_type(StringShm(paramDesc->GetName(), m_sharedMemory->get_segment_manager()),
                                                                              ParameterDescriptionShm(paramDesc, m_sharedMemory.get()))).first->second);
                    cdShm->AddOwnParameter(paramPtr);
                }
            }

            //Setup baseclasses and descendants and propertyMappings
            for (ClassMapShm::iterator it=m_repository->m_classes.begin(); it!=m_repository->m_classes.end(); ++it)
            {
                const ClassDescription* cd=localRepository->GetClass(it->first);
                ClassDescriptionShm& cdShm=it->second;
                if (cd->GetBaseClass())
                {
                    ClassDescriptionShm* baseShm=&(m_repository->m_classes.find(cd->GetBaseClass()->GetTypeId())->second);
                    cdShm.SetBaseClass(baseShm);
                    baseShm->AddDescendant(&cdShm);
                }

                //propertyMappings
                typeIdVec.clear();
                cd->GetPropertyIds(typeIdVec);
                for (std::set<DotsC_TypeId>::const_iterator propIdIt=typeIdVec.begin(); propIdIt!=typeIdVec.end(); ++propIdIt)
                {
                    bool inherited=false;
                    const PropertyMappingDescription* pm=cd->GetPropertyMapping(*propIdIt, inherited);

                    if (!inherited)
                    {
                        PropertyMappingDescriptionShm pmShm(pm,
                                                            &cdShm,
                                                            &(m_repository->m_properties.find(pm->GetProperty()->GetTypeId())->second),
                                                            m_sharedMemory.get());

                        for (int i=0; i<pm->GetProperty()->GetNumberOfMembers(); ++i)
                        {
                            const MemberMappingDescription* mm=pm->GetMemberMapping(i);
                            MemberMappingDescriptionShm mmShm(mm, m_sharedMemory.get());
                            if (mm->GetMappingKind()==MappedToParameter)
                            {
                                std::pair<const ParameterDescription*, int> paramAndIndex=mm->GetParameter();
                                mmShm.SetParamRef(&(m_repository->m_params.find(StringShm(paramAndIndex.first->GetName(), m_sharedMemory->get_segment_manager()))->second), paramAndIndex.second);
                            }

                            pmShm.AddMemberMapping(mmShm);
                        }

                        cdShm.AddPropertyMapping(pmShm);
                    }
                }
            }
        }
        catch (const boost::interprocess::interprocess_exception&)
        {
            SEND_SYSTEM_LOG(Error, << "Ran out of shared memory while loading types and parameters." <<std::endl
                            << "Please increase the sharemd memory size specified in typesystem.ini");
            localRepository.reset();
            m_repository=NULL;
        }
        catch (const std::exception& ex)
        {
            std::wcout<<"std "<<ex.what()<<std::endl;
            localRepository.reset();
            m_repository=NULL;
        }
    }

    ParameterDescriptionShm::ParameterDescriptionShm(const ParameterDescription* pd, boost::interprocess::managed_shared_memory* shm)
        :m_name(pd->GetName(), shm->get_segment_manager())
        ,m_memberType(pd->GetMemberType())
        ,m_isArray(pd->IsArray())
        ,m_hidden(pd->IsHidden())
        ,m_typeId(pd->GetTypeId())
        ,m_values(shm->get_segment_manager())
    {
        switch(m_memberType)
        {
        case BooleanMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.boolVal=pd->GetBoolValue(i);
                m_values.push_back(vd);
            }
        }
            break;

        case Int32MemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.int32Val=pd->GetInt32Value(i);
                m_values.push_back(vd);
            }
        }
            break;
        case Int64MemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.int64Val=pd->GetInt64Value(i);
                m_values.push_back(vd);
            }
        }
            break;

        case EntityIdMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                std::pair<boost::int64_t, const char*> hashed=pd->GetHashedValue(i);
                ValueDefinitionShm vd(hashed.second, shm);
                vd.hashedVal=hashed.first;
                vd.int64Val=pd->GetInt64Value(i);
                m_values.push_back(vd);
            }
        }
            break;
        case TypeIdMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.int64Val=pd->GetInt64Value(i);
                m_values.push_back(vd);
            }
        }
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                std::pair<boost::int64_t, const char*> hashed=pd->GetHashedValue(i);
                ValueDefinitionShm vd(hashed.second, shm);
                vd.hashedVal=hashed.first;
                m_values.push_back(vd);
            }
        }
            break;

        case StringMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                const char* str=pd->GetStringValue(i);
                ValueDefinitionShm vd(str, shm);
                m_values.push_back(vd);
            }
        }
            break;

        case ObjectMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                std::pair<const char*, size_t> bin=pd->GetObjectValue(i);
                ValueDefinitionShm vd(bin.first, bin.second, shm);
                m_values.push_back(vd);
            }
        }
            break;

        case EnumerationMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.int32Val=pd->GetInt32Value(i);
                m_values.push_back(vd);
            }
        }
            break;

        case BinaryMemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                std::pair<const char*, size_t> bin=pd->GetBinaryValue(i);
                ValueDefinitionShm vd(bin.first, bin.second, shm);
                m_values.push_back(vd);
            }
        }
            break;

        case Float32MemberType:
        case Ampere32MemberType:
        case CubicMeter32MemberType:
        case Hertz32MemberType:
        case Joule32MemberType:
        case Kelvin32MemberType:
        case Kilogram32MemberType:
        case Meter32MemberType:
        case MeterPerSecond32MemberType:
        case MeterPerSecondSquared32MemberType:
        case Newton32MemberType:
        case Pascal32MemberType:
        case Radian32MemberType:
        case RadianPerSecond32MemberType:
        case RadianPerSecondSquared32MemberType:
        case Second32MemberType:
        case SquareMeter32MemberType:
        case Steradian32MemberType:
        case Volt32MemberType:
        case Watt32MemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.float32Val=pd->GetFloat32Value(i);
                m_values.push_back(vd);
            }
        }
            break;

        case Float64MemberType:
        case Ampere64MemberType:
        case CubicMeter64MemberType:
        case Hertz64MemberType:
        case Joule64MemberType:
        case Kelvin64MemberType:
        case Kilogram64MemberType:
        case Meter64MemberType:
        case MeterPerSecond64MemberType:
        case MeterPerSecondSquared64MemberType:
        case Newton64MemberType:
        case Pascal64MemberType:
        case Radian64MemberType:
        case RadianPerSecond64MemberType:
        case RadianPerSecondSquared64MemberType:
        case Second64MemberType:
        case SquareMeter64MemberType:
        case Steradian64MemberType:
        case Volt64MemberType:
        case Watt64MemberType:
        {
            for (int i=0; i<pd->GetArraySize(); ++i)
            {
                ValueDefinitionShm vd(shm);
                vd.float64Val=pd->GetFloat64Value(i);
                m_values.push_back(vd);
            }
        }
            break;
        }
    }
}
}
}
}
