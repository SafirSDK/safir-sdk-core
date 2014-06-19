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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#ifndef __DOTS_INTERNAL_BASIC_TYPE_OPERATIONS_H__
#define __DOTS_INTERNAL_BASIC_TYPE_OPERATIONS_H__

#include <string>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/InternalDefs.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
namespace Internal
{
namespace BasicTypeOperations
{
    struct EnumerationMemberTypeName {static const std::string& Get() {static const std::string n="Enumeration"; return n;}};
    struct TypeIdMemberTypeName {static const std::string& Get() {static const std::string n="TypeId"; return n;}};
    struct EntityIdMemberTypeName {static const std::string& Get() {static const std::string n="EntityId"; return n;}};
    struct BooleanMemberTypeName {static const std::string& Get() {static const std::string n="Boolean"; return n;}};
    struct Int32MemberTypeName {static const std::string& Get() {static const std::string n="Int32"; return n;}};
    struct Int64MemberTypeName {static const std::string& Get() {static const std::string n="Int64"; return n;}};
    struct Float32MemberTypeName {static const std::string& Get() {static const std::string n="Float32"; return n;}};
    struct Float64MemberTypeName {static const std::string& Get() {static const std::string n="Float64"; return n;}};
    struct InstanceIdMemberTypeName {static const std::string& Get() {static const std::string n="InstanceId"; return n;}};
    struct ChannelIdMemberTypeName {static const std::string& Get() {static const std::string n="ChannelId"; return n;}};
    struct HandlerIdMemberTypeName {static const std::string& Get() {static const std::string n="HandlerId"; return n;}};
    struct StringMemberTypeName {static const std::string& Get() {static const std::string n="String"; return n;}};
    struct ObjectMemberTypeName {static const std::string& Get() {static const std::string n="Object"; return n;}};
    struct BinaryMemberTypeName {static const std::string& Get() {static const std::string n="Binary"; return n;}};
    struct Ampere32MemberTypeName {static const std::string& Get() {static const std::string n="Ampere32"; return n;}};
    struct CubicMeter32MemberTypeName {static const std::string& Get() {static const std::string n="CubicMeter32"; return n;}};
    struct Hertz32MemberTypeName {static const std::string& Get() {static const std::string n="Hertz32"; return n;}};
    struct Joule32MemberTypeName {static const std::string& Get() {static const std::string n="Joule32"; return n;}};
    struct Kelvin32MemberTypeName {static const std::string& Get() {static const std::string n="Kelvin32"; return n;}};
    struct Kilogram32MemberTypeName {static const std::string& Get() {static const std::string n="Kilogram32"; return n;}};
    struct Meter32MemberTypeName {static const std::string& Get() {static const std::string n="Meter32"; return n;}};
    struct MeterPerSecond32MemberTypeName {static const std::string& Get() {static const std::string n="MeterPerSecond32"; return n;}};
    struct MeterPerSecondSquared32MemberTypeName {static const std::string& Get() {static const std::string n="MeterPerSecondSquared32"; return n;}};
    struct Newton32MemberTypeName {static const std::string& Get() {static const std::string n="Newton32"; return n;}};
    struct Pascal32MemberTypeName {static const std::string& Get() {static const std::string n="Pascal32"; return n;}};
    struct Radian32MemberTypeName {static const std::string& Get() {static const std::string n="Radian32"; return n;}};
    struct RadianPerSecond32MemberTypeName {static const std::string& Get() {static const std::string n="RadianPerSecond32"; return n;}};
    struct RadianPerSecondSquared32MemberTypeName {static const std::string& Get() {static const std::string n="RadianPerSecondSquared32"; return n;}};
    struct Second32MemberTypeName {static const std::string& Get() {static const std::string n="Second32"; return n;}};
    struct SquareMeter32MemberTypeName {static const std::string& Get() {static const std::string n="SquareMeter32"; return n;}};
    struct Steradian32MemberTypeName {static const std::string& Get() {static const std::string n="Steradian32"; return n;}};
    struct Volt32MemberTypeName {static const std::string& Get() {static const std::string n="Volt32"; return n;}};
    struct Watt32MemberTypeName {static const std::string& Get() {static const std::string n="Watt32"; return n;}};
    struct Ampere64MemberTypeName {static const std::string& Get() {static const std::string n="Ampere64"; return n;}};
    struct CubicMeter64MemberTypeName {static const std::string& Get() {static const std::string n="CubicMeter64"; return n;}};
    struct Hertz64MemberTypeName {static const std::string& Get() {static const std::string n="Hertz64"; return n;}};
    struct Joule64MemberTypeName {static const std::string& Get() {static const std::string n="Joule64"; return n;}};
    struct Kelvin64MemberTypeName {static const std::string& Get() {static const std::string n="Kelvin64"; return n;}};
    struct Kilogram64MemberTypeName {static const std::string& Get() {static const std::string n="Kilogram64"; return n;}};
    struct Meter64MemberTypeName {static const std::string& Get() {static const std::string n="Meter64"; return n;}};
    struct MeterPerSecond64MemberTypeName {static const std::string& Get() {static const std::string n="MeterPerSecond64"; return n;}};
    struct MeterPerSecondSquared64MemberTypeName {static const std::string& Get() {static const std::string n="MeterPerSecondSquared64"; return n;}};
    struct Newton64MemberTypeName {static const std::string& Get() {static const std::string n="Newton64"; return n;}};
    struct Pascal64MemberTypeName {static const std::string& Get() {static const std::string n="Pascal64"; return n;}};
    struct Radian64MemberTypeName {static const std::string& Get() {static const std::string n="Radian64"; return n;}};
    struct RadianPerSecond64MemberTypeName {static const std::string& Get() {static const std::string n="RadianPerSecond64"; return n;}};
    struct RadianPerSecondSquared64MemberTypeName {static const std::string& Get() {static const std::string n="RadianPerSecondSquared64"; return n;}};
    struct Second64MemberTypeName {static const std::string& Get() {static const std::string n="Second64"; return n;}};
    struct SquareMeter64MemberTypeName {static const std::string& Get() {static const std::string n="SquareMeter64"; return n;}};
    struct Steradian64MemberTypeName {static const std::string& Get() {static const std::string n="Steradian64"; return n;}};
    struct Volt64MemberTypeName {static const std::string& Get() {static const std::string n="Volt64"; return n;}};
    struct Watt64MemberTypeName {static const std::string& Get() {static const std::string n="Watt64"; return n;}};

    /**
     * @brief Converts member type to string
     * @param type [in] - Get string representation of the member type. Note that Object and Enum are not looked up by this function.
     * @return name of member type
     */
    inline const std::string& MemberTypeToString(DotsC_MemberType type)
    {
        switch(type)
        {
        case EnumerationMemberType: return EnumerationMemberTypeName::Get();
        case TypeIdMemberType: return TypeIdMemberTypeName::Get();
        case EntityIdMemberType: return EntityIdMemberTypeName::Get();
        case BooleanMemberType: return BooleanMemberTypeName::Get();
        case Int32MemberType: return Int32MemberTypeName::Get();
        case Int64MemberType: return Int64MemberTypeName::Get();
        case Float32MemberType: return Float32MemberTypeName::Get();
        case Float64MemberType: return Float64MemberTypeName::Get();
        case InstanceIdMemberType: return InstanceIdMemberTypeName::Get();
        case ChannelIdMemberType: return ChannelIdMemberTypeName::Get();
        case HandlerIdMemberType: return HandlerIdMemberTypeName::Get();
        case StringMemberType: return StringMemberTypeName::Get();
        case ObjectMemberType: return ObjectMemberTypeName::Get();
        case BinaryMemberType: return BinaryMemberTypeName::Get();
        case Ampere32MemberType: return Ampere32MemberTypeName::Get();
        case CubicMeter32MemberType: return CubicMeter32MemberTypeName::Get();
        case Hertz32MemberType: return Hertz32MemberTypeName::Get();
        case Joule32MemberType: return Joule32MemberTypeName::Get();
        case Kelvin32MemberType: return Kelvin32MemberTypeName::Get();
        case Kilogram32MemberType: return Kilogram32MemberTypeName::Get();
        case Meter32MemberType: return Meter32MemberTypeName::Get();
        case MeterPerSecond32MemberType: return MeterPerSecond32MemberTypeName::Get();
        case MeterPerSecondSquared32MemberType: return MeterPerSecondSquared32MemberTypeName::Get();
        case Newton32MemberType: return Newton32MemberTypeName::Get();
        case Pascal32MemberType: return Pascal32MemberTypeName::Get();
        case Radian32MemberType: return Radian32MemberTypeName::Get();
        case RadianPerSecond32MemberType: return RadianPerSecond32MemberTypeName::Get();
        case RadianPerSecondSquared32MemberType: return RadianPerSecondSquared32MemberTypeName::Get();
        case Second32MemberType: return Second32MemberTypeName::Get();
        case SquareMeter32MemberType: return SquareMeter32MemberTypeName::Get();
        case Steradian32MemberType: return Steradian32MemberTypeName::Get();
        case Volt32MemberType: return Volt32MemberTypeName::Get();
        case Watt32MemberType: return Watt32MemberTypeName::Get();
        case Ampere64MemberType: return Ampere64MemberTypeName::Get();
        case CubicMeter64MemberType: return CubicMeter64MemberTypeName::Get();
        case Hertz64MemberType: return Hertz64MemberTypeName::Get();
        case Joule64MemberType: return Joule64MemberTypeName::Get();
        case Kelvin64MemberType: return Kelvin64MemberTypeName::Get();
        case Kilogram64MemberType: return Kilogram64MemberTypeName::Get();
        case Meter64MemberType: return Meter64MemberTypeName::Get();
        case MeterPerSecond64MemberType: return MeterPerSecond64MemberTypeName::Get();
        case MeterPerSecondSquared64MemberType: return MeterPerSecondSquared64MemberTypeName::Get();
        case Newton64MemberType: return Newton64MemberTypeName::Get();
        case Pascal64MemberType: return Pascal64MemberTypeName::Get();
        case Radian64MemberType: return Radian64MemberTypeName::Get();
        case RadianPerSecond64MemberType: return RadianPerSecond64MemberTypeName::Get();
        case RadianPerSecondSquared64MemberType: return RadianPerSecondSquared64MemberTypeName::Get();
        case Second64MemberType: return Second64MemberTypeName::Get();
        case SquareMeter64MemberType: return SquareMeter64MemberTypeName::Get();
        case Steradian64MemberType: return Steradian64MemberTypeName::Get();
        case Volt64MemberType: return Volt64MemberTypeName::Get();
        case Watt64MemberType: return Watt64MemberTypeName::Get();
        }

        throw std::invalid_argument("BasicTypeOperations::MemberTypeToString. The MemberType does not exist");
    }

    /**
     * @brief Looks up the memberType that corresponds to the typeName.
     * @param typeName [in] - name of a type
     * @return Corresponding memberType.
     */
    inline const DotsC_MemberType StringToMemberType(const std::string& typeName)
    {
        if (typeName==TypeIdMemberTypeName::Get()) return TypeIdMemberType;
        if (typeName==EntityIdMemberTypeName::Get()) return EntityIdMemberType;
        if (typeName==BooleanMemberTypeName::Get()) return BooleanMemberType;
        if (typeName==Int32MemberTypeName::Get()) return Int32MemberType;
        if (typeName==Int64MemberTypeName::Get()) return Int64MemberType;
        if (typeName==Float32MemberTypeName::Get()) return Float32MemberType;
        if (typeName==Float64MemberTypeName::Get()) return Float64MemberType;
        if (typeName==InstanceIdMemberTypeName::Get()) return InstanceIdMemberType;
        if (typeName==ChannelIdMemberTypeName::Get()) return ChannelIdMemberType;
        if (typeName==HandlerIdMemberTypeName::Get()) return HandlerIdMemberType;
        if (typeName==StringMemberTypeName::Get()) return StringMemberType;
        if (typeName==BinaryMemberTypeName::Get()) return BinaryMemberType;
        if (typeName==Ampere32MemberTypeName::Get()) return Ampere32MemberType;
        if (typeName==CubicMeter32MemberTypeName::Get()) return CubicMeter32MemberType;
        if (typeName==Hertz32MemberTypeName::Get()) return Hertz32MemberType;
        if (typeName==Joule32MemberTypeName::Get()) return Joule32MemberType;
        if (typeName==Kelvin32MemberTypeName::Get()) return Kelvin32MemberType;
        if (typeName==Kilogram32MemberTypeName::Get()) return Kilogram32MemberType;
        if (typeName==Meter32MemberTypeName::Get()) return Meter32MemberType;
        if (typeName==MeterPerSecond32MemberTypeName::Get()) return MeterPerSecond32MemberType;
        if (typeName==MeterPerSecondSquared32MemberTypeName::Get()) return MeterPerSecondSquared32MemberType;
        if (typeName==Newton32MemberTypeName::Get()) return Newton32MemberType;
        if (typeName==Pascal32MemberTypeName::Get()) return Pascal32MemberType;
        if (typeName==Radian32MemberTypeName::Get()) return Radian32MemberType;
        if (typeName==RadianPerSecond32MemberTypeName::Get()) return RadianPerSecond32MemberType;
        if (typeName==RadianPerSecondSquared32MemberTypeName::Get()) return RadianPerSecondSquared32MemberType;
        if (typeName==Second32MemberTypeName::Get()) return Second32MemberType;
        if (typeName==SquareMeter32MemberTypeName::Get()) return SquareMeter32MemberType;
        if (typeName==Steradian32MemberTypeName::Get()) return Steradian32MemberType;
        if (typeName==Volt32MemberTypeName::Get()) return Volt32MemberType;
        if (typeName==Watt32MemberTypeName::Get()) return Watt32MemberType;
        if (typeName==Ampere64MemberTypeName::Get()) return Ampere64MemberType;
        if (typeName==CubicMeter64MemberTypeName::Get()) return CubicMeter64MemberType;
        if (typeName==Hertz64MemberTypeName::Get()) return Hertz64MemberType;
        if (typeName==Joule64MemberTypeName::Get()) return Joule64MemberType;
        if (typeName==Kelvin64MemberTypeName::Get()) return Kelvin64MemberType;
        if (typeName==Kilogram64MemberTypeName::Get()) return Kilogram64MemberType;
        if (typeName==Meter64MemberTypeName::Get()) return Meter64MemberType;
        if (typeName==MeterPerSecond64MemberTypeName::Get()) return MeterPerSecond64MemberType;
        if (typeName==MeterPerSecondSquared64MemberTypeName::Get()) return MeterPerSecondSquared64MemberType;
        if (typeName==Newton64MemberTypeName::Get()) return Newton64MemberType;
        if (typeName==Pascal64MemberTypeName::Get()) return Pascal64MemberType;
        if (typeName==Radian64MemberTypeName::Get()) return Radian64MemberType;
        if (typeName==RadianPerSecond64MemberTypeName::Get()) return RadianPerSecond64MemberType;
        if (typeName==RadianPerSecondSquared64MemberTypeName::Get()) return RadianPerSecondSquared64MemberType;
        if (typeName==Second64MemberTypeName::Get()) return Second64MemberType;
        if (typeName==SquareMeter64MemberTypeName::Get()) return SquareMeter64MemberType;
        if (typeName==Steradian64MemberTypeName::Get()) return Steradian64MemberType;
        if (typeName==Volt64MemberTypeName::Get()) return Volt64MemberType;
        if (typeName==Watt64MemberTypeName::Get()) return Watt64MemberType;

        //some strange string can be an object or an enum. Caller must investigate further what it is
        if (typeName=="Enumeration") return EnumerationMemberType;
        if (typeName=="Object") return ObjectMemberType;
        return ObjectMemberType;
    }

    /**
     * @brief Check if memberType is a basic type (built in type) or a complex type (user defined).
     * @param memberType [in] - memberType to check if it is a basic type, i.e a non user defined type.
     * @return True if memberType is a basic type.
     */
    inline bool IsBasicMemberType(DotsC_MemberType memberType)
    {
        return (memberType!=ObjectMemberType && memberType!=EnumerationMemberType); //Object and Enum is not basic types
    }

    /**
     * @brief Check if typeName represents a basic type. In that case the memberType is also returned in out parameter 'memberType'
     * @param typeName [in] - name of a type
     * @param memberType [out] - if typeName represents a basic type, the memberType is returned here
     * @return True if typeName is a basic type.
     */
    inline bool IsBasicTypeName(const std::string& typeName, DotsC_MemberType& memberType)
    {
        memberType=StringToMemberType(typeName);
        return IsBasicMemberType(memberType);
    }

    template <class RepositoryT>
    bool ValidTypeId(const RepositoryT* repository, DotsC_TypeId tid)
    {
        return repository->GetClass(tid)!=NULL ||
                repository->GetEnum(tid)!=NULL ||
                repository->GetProperty(tid)!=NULL ||
                repository->GetException(tid)!=NULL;
    }

    template <class RepT, class Traits=Safir::Dob::Typesystem::ToolSupport::TypeRepositoryTraits<RepT> >
    struct BasicTypeOperationHelper
    {
        typedef typename Traits::RepositoryType RepositoryType;
        typedef typename Traits::ClassDescriptionType ClassDescriptionType;
        typedef typename Traits::EnumDescriptionType EnumDescriptionType;
        typedef typename Traits::PropertyDescriptionType PropertyDescriptionType;
        typedef typename Traits::ExceptionDescriptionType ExceptionDescriptionType;

        bool IsOfType(const RepositoryType* repository, DotsC_MemberType mt, DotsC_TypeId tid, DotsC_MemberType ofMt, DotsC_TypeId ofTid) const
        {
            if (mt!=ofMt)
            {
                return false;
            }
            else if (mt==EnumerationMemberType)
            {
                return tid==ofTid;
            }
            else if (mt==ObjectMemberType)
            {
                //Check object types and handle inheritance.
                const ClassDescriptionType* tmpClass=repository->GetClass(tid);
                while (tmpClass)
                {
                    if (tmpClass->GetTypeId()==ofTid)
                        return true;

                    tmpClass=tmpClass->GetBaseClass();
                }
                return false;
            }
            else //Basic types
            {
                return true;
            }
        }

        const char* TypeIdToTypeName(const RepositoryType* repository, DotsC_TypeId tid) const
        {
            const ClassDescriptionType* cd=repository->GetClass(tid);
            if (cd)
            {
                return cd->GetName();
            }
            const EnumDescriptionType* ed=repository->GetEnum(tid);
            if (ed)
            {
                return ed->GetName();
            }
            const PropertyDescriptionType* pd=repository->GetProperty(tid);
            if (pd)
            {
                return pd->GetName();
            }
            const ExceptionDescriptionType* ex=repository->GetException(tid);
            if (ex)
            {
                return ex->GetName();
            }
            return NULL;
        }
    };

    template <class RepositoryT>
    bool IsOfType(const RepositoryT* repository, DotsC_MemberType mt, DotsC_TypeId tid, DotsC_MemberType ofMt, DotsC_TypeId ofTid)
    {
        BasicTypeOperationHelper<RepositoryT> helper;
        return helper.IsOfType(repository, mt, tid, ofMt, ofTid);
    }

    template <class RepositoryT>
    const char* TypeIdToTypeName(const RepositoryT* repository, DotsC_TypeId tid)
    {
        BasicTypeOperationHelper<RepositoryT> helper;
        return helper.TypeIdToTypeName(repository, tid);
    }

    inline std::string CollectionTypeToString(DotsC_CollectionType collectionType)
    {
        switch (collectionType)
        {
        case SingleValueCollectionType: return "single_value";
        case ArrayCollectionType: return "array";
        case SequenceCollectionType: return "sequence";
        case DictionaryCollectionType: return "dictionary";
        }
        return "";
    }

    struct PredefindedClassNames
    {
        static const std::string& ObjectName() {static const std::string s("Object"); return s;}
        static const std::string& ExceptionName() {static const std::string s("Exception"); return s;}
        static const std::string& FundamentalExceptionName() {static const std::string s("FundamentalException"); return s;}
        static const std::string& NullExceptionName() {static const std::string s("Safir.Dob.Typesystem.NullException"); return s;}
        static const std::string& IncompatibleTypesExceptionName() {static const std::string s("Safir.Dob.Typesystem.IncompatibleTypesException"); return s;}
        static const std::string& ReadOnlyExceptionName() {static const std::string s("Safir.Dob.Typesystem.ReadOnlyException"); return s;}
        static const std::string& IllegalValueExceptionName() {static const std::string s("Safir.Dob.Typesystem.IllegalValueException"); return s;}
        static const std::string& SoftwareViolationExceptionName() {static const std::string s("Safir.Dob.Typesystem.SoftwareViolationException"); return s;}
        static const std::string& ConfigurationErrorExceptionName() {static const std::string s("Safir.Dob.Typesystem.ConfigurationErrorException"); return s;}
    };

    /**
     * Helper class for getting index corresponiding to a specific key. Only applicable on dictionaries.
     */
    template <class ParameterDescriptionT, class KeyT> struct DictionaryKeyToIndexHelper;
    template <class ParameterDescriptionT> struct DictionaryKeyToIndexHelper<ParameterDescriptionT, std::string>
    {
        static int Index(const ParameterDescriptionT* pd, const std::string& key)
        {
            for (int i=0; i<pd->GetNumberOfValues(); ++i)
            {
                if (key==pd->GetStringKey(i))
                    return i;
            }
            return -1;
        }
    };
    template <class ParameterDescriptionT> struct DictionaryKeyToIndexHelper<ParameterDescriptionT, DotsC_Int32>
    {
        static int Index(const ParameterDescriptionT* pd, DotsC_Int32 key)
        {
            for (int i=0; i<pd->GetNumberOfValues(); ++i)
            {
                if (key==pd->GetInt32Key(i))
                    return i;
            }
            return -1;
        }

    };
    template <class ParameterDescriptionT> struct DictionaryKeyToIndexHelper<ParameterDescriptionT, DotsC_Int64>
    {
        static int Index(const ParameterDescriptionT* pd, DotsC_Int64 key)
        {
            for (int i=0; i<pd->GetNumberOfValues(); ++i)
            {
                if (key==pd->GetInt64Key(i))
                    return i;
            }
            return -1;
        }

    };
    template <class ParameterDescriptionT> struct DictionaryKeyToIndexHelper<ParameterDescriptionT, std::pair<DotsC_TypeId, DotsC_Int64> >
    {
        static int Index(const ParameterDescriptionT* pd, std::pair<DotsC_TypeId, DotsC_Int64> key)
        {
            for (int i=0; i<pd->GetNumberOfValues(); ++i)
            {
                if (key.first==pd->GetInt64Key(i) && key.second==pd->GetHashedKey(i).first)
                    return i;
            }
            return -1;
        }
    };
    template <class ParameterDescriptionT> struct DictionaryKeyToIndexHelper<ParameterDescriptionT, std::pair<DotsC_Int64, const char*> >
    {
        static int Index(const ParameterDescriptionT* pd, std::pair<DotsC_Int64, const char*> key)
        {
            for (int i=0; i<pd->GetNumberOfValues(); ++i)
            {
                if (key.first==pd->GetHashedKey(i).first)
                    return i;
            }
            return -1;
        }
    };

}
}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal::BasicTypeOperations

#endif


