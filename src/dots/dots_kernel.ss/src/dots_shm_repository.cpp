/******************************************************************************
*
* Copyright Consoden AB, 2004-2015 (http://safir.sourceforge.net)
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
    void RepositoryShm::CreateShmCopyOfRepository(const Safir::Dob::Typesystem::ToolSupport::TypeRepository& srcRepository,
                                                  const std::string& shmRepositoryName,
                                                  boost::interprocess::managed_shared_memory& sharedMemory)
    {
        //Start copy content from src into repository
        try
        {
            //-----------------------------------------------------
            //Create RepositoryShm wich is container for all types
            //-----------------------------------------------------
            RepositoryShm* repository=sharedMemory.construct<RepositoryShm>(shmRepositoryName.c_str())(&sharedMemory);

            //----------------------------------
            //Enums
            //----------------------------------
            std::set<DotsC_TypeId> typeIdVec;
            srcRepository.GetAllEnumTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                repository->m_enums.insert(EnumMapShm::value_type(*it, EnumDescriptionShm(srcRepository.GetEnum(*it), &sharedMemory)));
            }

            //----------------------------------
            //Exceptions
            //----------------------------------
            typeIdVec.clear();
            srcRepository.GetAllExceptionTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                repository->m_exceptions.insert(ExceptionMapShm::value_type(*it, ExceptionDescriptionShm(srcRepository.GetException(*it), &sharedMemory)));
            }
            for (ExceptionMapShm::iterator it=repository->m_exceptions.begin(); it!=repository->m_exceptions.end(); ++it)
            {
                const ExceptionDescription* base=srcRepository.GetException(it->first)->GetBaseClass();
                if (base)
                {
                    it->second.SetBaseClass(&(repository->m_exceptions.find(base->GetTypeId())->second));
                    //
                }
            }

            //----------------------------------
            //Properties
            //----------------------------------
            typeIdVec.clear();
            srcRepository.GetAllPropertyTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                repository->m_properties.insert(PropertyMapShm::value_type(*it, PropertyDescriptionShm(srcRepository.GetProperty(*it), &sharedMemory)));
            }

            //--------------------------------------------
            //Classes, parameters and propertyMappings
            //--------------------------------------------
            typeIdVec.clear();
            srcRepository.GetAllClassTypeIds(typeIdVec);
            for (std::set<DotsC_TypeId>::const_iterator it=typeIdVec.begin(); it!=typeIdVec.end(); ++it)
            {
                //class
                const ClassDescription* cd=srcRepository.GetClass(*it);
                ClassDescriptionShm* cdShm= &(repository->m_classes.insert(ClassMapShm::value_type(*it, ClassDescriptionShm(cd, &sharedMemory))).first->second);

                //parameters
                int numParams=cd->GetNumberOfParameters();
                int startParam=numParams-cd->GetNumberOfOwnParameters();
                for (int i=startParam; i<numParams; ++i)
                {
                    const ParameterDescription* paramDesc=cd->GetParameter(i);
                    ParameterDescriptionShmPtr paramPtr=&(repository->m_params.insert(ParameterMapShm::value_type(StringShm(paramDesc->GetQualifiedName(), sharedMemory.get_segment_manager()),
                                                                              ParameterDescriptionShm(paramDesc, &sharedMemory))).first->second);
                    cdShm->AddOwnParameter(paramPtr);
                }
            }

            //Setup baseclasses and descendants and propertyMappings
            for (ClassMapShm::iterator it=repository->m_classes.begin(); it!=repository->m_classes.end(); ++it)
            {
                const ClassDescription* cd=srcRepository.GetClass(it->first);
                ClassDescriptionShm& cdShm=it->second;
                if (cd->GetBaseClass())
                {
                    ClassDescriptionShm* baseShm=&(repository->m_classes.find(cd->GetBaseClass()->GetTypeId())->second);
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
                                                            &(repository->m_properties.find(pm->GetProperty()->GetTypeId())->second),
                                                            &sharedMemory);

                        for (int i=0; i<pm->GetProperty()->GetNumberOfMembers(); ++i)
                        {
                            const MemberMappingDescription* mm=pm->GetMemberMapping(i);
                            MemberMappingDescriptionShm mmShm(mm, &sharedMemory);
                            if (mm->GetMappingKind()==MappedToParameter)
                            {
                                std::pair<const ParameterDescription*, int> paramAndIndex=mm->GetParameter();
                                mmShm.SetParamRef(&(repository->m_params.find(StringShm(paramAndIndex.first->GetQualifiedName(), sharedMemory.get_segment_manager()))->second), paramAndIndex.second);
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
        }
        catch (const std::exception& ex)
        {
            SEND_SYSTEM_LOG(Error, << "Failure while loading types and parameters into shared memory" <<std::endl
                            << ex.what());
        }

    }

    ParameterDescriptionShm::ParameterDescriptionShm(const ParameterDescription* pd, boost::interprocess::managed_shared_memory* shm)
        :m_name(pd->GetName(), shm->get_segment_manager())
        ,m_qualifiedName(pd->GetQualifiedName(), shm->get_segment_manager())
        ,m_memberType(pd->GetMemberType())
        ,m_collectionType(pd->GetCollectionType())
        ,m_keyType(pd->GetKeyType())
        ,m_hidden(pd->IsHidden())
        ,m_typeId(pd->GetTypeId())
        ,m_keyTypeId(pd->GetKeyTypeId())
        ,m_values(shm->get_segment_manager())
        ,m_unifiedKeyToIndex(std::less<const DotsC_Int64>(), shm->get_segment_manager())
    {
        switch(m_memberType)
        {
        case BooleanMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.boolean=pd->GetBoolValue(i);
                    m_values.push_back(vd);
                }
            }
            break;

        case Int32MemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.int32=pd->GetInt32Value(i);
                    m_values.push_back(vd);
                }
            }
            break;
        case Int64MemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.int64=pd->GetInt64Value(i);
                    m_values.push_back(vd);
                }
            }
            break;

        case EntityIdMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    std::pair<boost::int64_t, const char*> hashed=pd->GetHashedValue(i);
                    if (hashed.second==NULL)
                    {
                        hashed.second="";
                    }
                    ValueDefinitionShm vd(hashed.second, shm);
                    vd.val.hash=hashed.first;
                    vd.val.int64=pd->GetInt64Value(i);
                    m_values.push_back(vd);
                }
            }
            break;
        case TypeIdMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.int64=pd->GetInt64Value(i);
                    m_values.push_back(vd);
                }
            }
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    std::pair<boost::int64_t, const char*> hashed=pd->GetHashedValue(i);
                    if (hashed.second==NULL)
                    {
                        hashed.second="";
                    }
                    ValueDefinitionShm vd(hashed.second, shm);
                    vd.val.hash=hashed.first;
                    m_values.push_back(vd);
                }
            }
            break;

        case StringMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    const char* str=pd->GetStringValue(i);
                    ValueDefinitionShm vd(str, shm);
                    m_values.push_back(vd);
                }
            }
            break;

        case ObjectMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    std::pair<const char*, size_t> bin=pd->GetObjectValue(i);
                    ValueDefinitionShm vd(bin.first, bin.second, shm);
                    m_values.push_back(vd);
                }
            }
            break;

        case EnumerationMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.int32=pd->GetInt32Value(i);
                    m_values.push_back(vd);
                }
            }
            break;

        case BinaryMemberType:
            {
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
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
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.float32=pd->GetFloat32Value(i);
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
                for (int i=0; i<pd->GetNumberOfValues(); ++i)
                {
                    ValueDefinitionShm vd(shm);
                    vd.val.float64=pd->GetFloat64Value(i);
                    m_values.push_back(vd);
                }
            }
            break;
        }

        if (m_collectionType==DictionaryCollectionType)
        {
            switch (m_keyType)
            {
            case Int32MemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.key.int32=pd->GetInt32Key(i);
                    }
                }
                break;
            case Int64MemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.key.int64=pd->GetInt64Key(i);
                    }
                }
                break;

            case EnumerationMemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.key.int32=pd->GetInt32Key(i);
                    }
                }
                break;

            case EntityIdMemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        std::pair<boost::int64_t, const char*> hashed=pd->GetHashedKey(i);
                        if (hashed.second==NULL)
                        {
                            hashed.second="";
                        }
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.stringKey=StringShm(hashed.second, shm->get_segment_manager());
                        vd.key.hash=hashed.first;
                        vd.key.int64=pd->GetInt64Key(i);
                    }
                }
                break;
            case TypeIdMemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.key.int64=pd->GetInt64Key(i);
                    }
                }
                break;
            case InstanceIdMemberType:
            case ChannelIdMemberType:
            case HandlerIdMemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        std::pair<boost::int64_t, const char*> hashed=pd->GetHashedKey(i);
                        if (hashed.second==NULL)
                        {
                            hashed.second="";
                        }
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.stringKey=StringShm(hashed.second, shm->get_segment_manager());
                        vd.key.hash=hashed.first;
                    }
                }
                break;

            case StringMemberType:
                {
                    for (int i=0; i<pd->GetNumberOfValues(); ++i)
                    {
                        const char* str=pd->GetStringKey(i);
                        ValueDefinitionShm& vd=m_values[static_cast<size_t>(i)];
                        vd.stringKey=StringShm(str, shm->get_segment_manager());
                    }
                }
                break;

            default:
                break;
            }

            //copy unified key to index map
            m_unifiedKeyToIndex.insert(pd->UnifiedKeyToIndexMap().begin(), pd->UnifiedKeyToIndexMap().end());
        }
    }
}
}
}
}
