/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
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

#include "dots_parameter_database.h"
#include "dots_basic_types.h"
#include "dots_error_handler.h"
#include "dots_blob_serializer.h"
#include "dots_blob_layout.h"
#include "dots_xml_serializer.h"
#include "dots_base64_conversions.h"
#include <iostream>
#include <Safir/Dob/Typesystem/Internal/Id.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    static const char* PARAMETERS_DICTIONARY_NAME = "PARAMETER_BLOB";

    template <class T>
    void Write(T& dest, const T source)
    {
#ifdef NO_UNALIGNED_ACCESS
        memcpy(&dest, &source, sizeof(T));
#else
        dest = source;
#endif
    }

    /*
    void ParameterDatabase::PrintOffset(const std::string & desc, const ParameterOffset & offset) const
    {
        std::wcout << desc.c_str() << " = " << std::hex << "0x"
                   << offset.get() - static_cast<const char *>(m_parameterBlob)
                   << std::endl;
                   }*/


    ParameterDatabase::ParameterDatabase(create_and_initialize_t,
                                         const size_t parametersSize,
                                         AllocationHelper & allocHelper):
        m_parameterBlob(NULL)
    {
        if (parametersSize > 0)
        {
            m_parameterBlob = allocHelper.GetShmem()->construct<char>(PARAMETERS_DICTIONARY_NAME)[parametersSize]();
            m_firstFree = static_cast<char*>(m_parameterBlob);
        }
    }


    ParameterDatabase::ParameterDatabase(open_only_t,
                                         boost::interprocess::managed_shared_memory & shmem):
        m_parameterBlob(shmem.find<char>(PARAMETERS_DICTIONARY_NAME).first)
    {

    }

    ParameterDatabase::~ParameterDatabase()
    {
    }

    const ParameterOffsetConst
    ParameterDatabase::Insert(const DobParameter & param, //the parameter to insert
                              const EnumDatabase & enumDb, //enumeration database, in case param is an enum
                              AllocationHelper & allocHelper) //alloc helper in case allocation has to be done.
    {
        ParameterOffset paramStart = m_firstFree;

        switch (param.m_type)
        {
        case BooleanMemberType:
            Ibool(param, allocHelper);
            break;
        case EnumerationMemberType:
            Ienum(param,enumDb,allocHelper);
            break;
        case Int32MemberType:
            Iint32(param, allocHelper);
            break;
        case Int64MemberType:
            Iint64(param, allocHelper);
            break;
        case Float32MemberType:
            Ifloat32(param, allocHelper);
            break;
        case Float64MemberType:
            Ifloat64(param, allocHelper);
            break;
        case TypeIdMemberType:
            Itid(param, allocHelper);
            break;
        case EntityIdMemberType:
            IentityId(param, allocHelper);
            break;
        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
            InsertHashedType(param,allocHelper);
            break;
        case StringMemberType:
            Istring(param, allocHelper);
            break;
        case ObjectMemberType:
            Iobject(param, allocHelper);
            break;
        case BinaryMemberType:
            Ibinary(param, allocHelper);
            break;

        //SI Types
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
            Ifloat32(param, allocHelper);
            break;

        //SI Types
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
            Ifloat64(param, allocHelper);
            break;

            /*default:
            std::string descr="The type '";
            descr+=BasicTypes::StringOf(param.m_type);
            descr+="' is not a supported parameter type in this version";
            ErrorHandler::Error("Unsupported parameter type", descr, "dots_parameter_database");
            break;*/
        }

        return paramStart;
    }


    void ParameterDatabase::Ibool(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        const size_t size = BasicTypes::SizeOfType(val.m_type);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            boost::interprocess::offset_ptr<bool> b = ParameterOffsetCast<bool>(m_firstFree);
            //bool* b=reinterpret_cast<bool*>(m_parameters+m_offset);
            if (eq(val.m_values[ix].m_value.c_str(), "true") || eq(val.m_values[ix].m_value.c_str(), "TRUE") || eq(val.m_values[ix].m_value.c_str(), "True"))
                *b=true;
            else if (eq(val.m_values[ix].m_value.c_str(), "false") || eq(val.m_values[ix].m_value.c_str(), "FALSE") || eq(val.m_values[ix].m_value.c_str(), "False"))
                *b=false;
            else
            {
                std::string descr="No bool value: '";
                descr+=val.m_values[ix].m_value;
                Error(val, ix, descr.c_str());

            }
            m_firstFree += size;
        }
    }

    void ParameterDatabase::Ienum(const DobParameter& val, const EnumDatabase & enumDb, AllocationHelper & /*allocHelper*/)
    {
        const EnumDescription * ed=enumDb.FindEnum(val.m_objType);
        if (ed == NULL)
        {
            std::ostringstream ostr;
            ostr << "Failed to get enumeration information for type "
                 << val.m_objType << ". Parameter name = "
                 << val.m_name << ": " << val.m_file;

            throw InternalException(ostr.str(),__FILE__,__LINE__);
        }

        const size_t size=BasicTypes::SizeOfType(val.m_type);

        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            int enumVal=ed->IndexOf(val.m_values[ix].m_value.c_str());
            if (enumVal == -1)
            {
                std::string descr="The value '";
                descr+=val.m_values[ix].m_value;
                descr+="' is not a value of the type ";
                descr+=ed->Name();
                Error(val, ix, descr);

            }

            Write(*ParameterOffsetCast<Int32>(m_firstFree),enumVal);
            m_firstFree+=size;
        }
    }

    void ParameterDatabase::Iint32(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        const size_t size = BasicTypes::SizeOfType(val.m_type);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            try
            {
                Write(*ParameterOffsetCast<Int32>(m_firstFree), 
                      boost::lexical_cast<Int32>(val.m_values[ix].m_value.c_str()));
                m_firstFree += size;
            }
            catch (boost::bad_lexical_cast &)
            {
                std::string desc = "Value can't be converted to Int32: " + val.m_values[ix].m_value;
                Error(val, ix, desc.c_str());

            }
        }
    }

    void ParameterDatabase::Iint64(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        const size_t size = BasicTypes::SizeOfType(val.m_type);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            try
            {
                Write(*ParameterOffsetCast<Int64>(m_firstFree),
                      boost::lexical_cast<Int64>(val.m_values[ix].m_value.c_str()));
                m_firstFree += size;
            }
            catch (boost::bad_lexical_cast &)
            {
                std::string desc = "Value can't be converted to Int64: " + val.m_values[ix].m_value;
                Error(val, ix, desc.c_str());

            }
        }
    }

    void ParameterDatabase::Ifloat32(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        const size_t size = BasicTypes::SizeOfType(val.m_type);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            try
            {
                Write(*ParameterOffsetCast<Float32>(m_firstFree), 
                      boost::lexical_cast<Float32>(val.m_values[ix].m_value.c_str()));
                m_firstFree += size;
            }
            catch (boost::bad_lexical_cast &)
            {
                std::string desc = "Value can't be converted to Float32: " + val.m_values[ix].m_value;
                Error(val, ix, desc.c_str());

            }
        }
    }

    void ParameterDatabase::Ifloat64(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        const size_t size = BasicTypes::SizeOfType(val.m_type);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            try
            {
                Write (*ParameterOffsetCast<Float64>(m_firstFree),
                       boost::lexical_cast<Float64>(val.m_values[ix].m_value.c_str()));
                m_firstFree += size;
            }
            catch (boost::bad_lexical_cast &)
            {
                std::string desc = "Value can't be converted to Float64: " + val.m_values[ix].m_value;
                Error(val, ix, desc.c_str());

            }
        }
    }

    void ParameterDatabase::Itid(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        const size_t size = BasicTypes::SizeOfType(val.m_type);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            TypeId tid;
            const bool success = XmlToBlobSerializer::ToTypeId(val.m_values[ix].m_value.c_str(), tid);
            if (!success)
            {
                std::string desc = "TypeId parameter of undefined type. No such type: " + val.m_values[ix].m_value;
                Error(val, ix, desc.c_str());

            }
            Write(*ParameterOffsetCast<TypeId>(m_firstFree),tid);
            m_firstFree += size;
        }
    }


    void ParameterDatabase::InsertHashedType(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        ParameterOffset nextString = m_firstFree + val.m_values.size()*sizeof(ParameterOffset);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            boost::interprocess::offset_ptr<ParameterOffset > currentOffset =
                ParameterOffsetCast<ParameterOffset>(m_firstFree);
            *currentOffset = nextString;

            Int64 hashVal;
            std::string strVal;
            if (IsInt(val.m_values[ix].m_value.c_str()))
            {
                hashVal = boost::lexical_cast<Int64>(val.m_values[ix].m_value.c_str());
            }
            else
            {
                hashVal = DotsId_Generate64(val.m_values[ix].m_value.c_str());
                strVal = val.m_values[ix].m_value;
            }

            Write(*ParameterOffsetCast<Int64>(nextString), hashVal);
            strcpy(nextString.get() + sizeof(Int64), strVal.c_str());
            nextString += strVal.size() + 1 + sizeof(Int64);
            m_firstFree += sizeof(ParameterOffset);
        }
        m_firstFree = nextString;
    }

    void ParameterDatabase::IentityId(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        ParameterOffset nextString = m_firstFree + val.m_values.size()*sizeof(ParameterOffset);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            boost::interprocess::offset_ptr<ParameterOffset > currentOffset =
                ParameterOffsetCast<ParameterOffset>(m_firstFree);
            *currentOffset = nextString;

            DotsC_EntityId entityId;
            std::string instanceIdStr;
            const bool success = XmlToBlobSerializer::ToEntityId(val.m_values[ix].m_value.c_str(), entityId, instanceIdStr);
            if (!success)
            {
                Error(val, ix, "EntityId contains undefined type");
                throw "No such type";
            }
            Write(*ParameterOffsetCast<DotsC_EntityId>(nextString), entityId);
            strcpy(nextString.get() + sizeof(DotsC_EntityId), instanceIdStr.c_str());
            nextString += instanceIdStr.size() + 1 + sizeof(DotsC_EntityId);
            m_firstFree += sizeof(ParameterOffset);
        }
        m_firstFree = nextString;
    }

    void ParameterDatabase::Istring(const DobParameter& val, AllocationHelper & /*allocHelper*/)
    {
        ParameterOffset nextString = m_firstFree + val.m_values.size()*sizeof(ParameterOffset);
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            boost::interprocess::offset_ptr<ParameterOffset > currentOffset =
                ParameterOffsetCast<ParameterOffset>(m_firstFree);
            *currentOffset = nextString;
            strcpy(nextString.get(), val.m_values[ix].m_value.c_str());
            nextString += val.m_values[ix].m_value.size() + 1;
            m_firstFree += sizeof(ParameterOffset);
        }
        m_firstFree = nextString;
    }

    void ParameterDatabase::Iobject(const DobParameter& val, AllocationHelper & allocHelper)
    {
        ParameterOffset before = m_firstFree;
        try
        {
            for (size_t ix=0; ix<val.m_values.size(); ix++)
            {
                const DobParameter::ParameterValue & theValue = val.m_values[ix];
                if (theValue.m_valueFromParameter)
                {
                    std::wcerr << "Cannot add index " << ix <<" of parameter " << val.m_name.c_str()
                               << " (m_valueFromParameter is true!). This is probably a DOB bug..."<< std::endl;
                    continue;
                    //references are not resolved here
                }
                else
                {
                    XmlToBlobSerializer xs;

                    char * tmp = xs.Serialize(theValue.m_value.c_str());
                    if (tmp==NULL)
                    {
                        Error(val, ix, "Failed to deserialize object from XML.");
                    }
                    ParameterOffset addr(static_cast<char*>(allocHelper.GetShmem()->allocate(BlobLayout::GetSize(tmp))));
                    memcpy(addr.get(), tmp, BlobLayout::GetSize(tmp));
                    boost::interprocess::offset_ptr<ParameterOffset> blob = ParameterOffsetCast<ParameterOffset>(m_firstFree);
                    *blob=addr;
                    BlobLayout::DeleteBlob(tmp);
                    m_firstFree+=sizeof(ParameterOffset);
                }
            }
        }
        catch (const DeserializationFailure &)
        {//an exception occurred, we roll back so that we can insert the parameter again later without failing...
            m_firstFree = before;
            throw;
        }
    }

    void ParameterDatabase::Ibinary(const DobParameter& val, AllocationHelper & allocHelper)
    {
        //        std::wcout << "ABAQ Inserting binary parameter " << val.m_name.c_str() << std::endl;
        for (size_t ix=0; ix<val.m_values.size(); ix++)
        {
            Int32 size = 0;
            const char* source = val.m_values[ix].m_value.c_str();
            const Int32 sourceSize = (Int32)val.m_values[ix].m_value.length();

            std::vector<char> bin(Base64Conversions::CalculateBinarySize(sourceSize));
            if (bin.size() != 0)
            {
                if (!Base64Conversions::ToBinary(&bin[0], static_cast<Int32>(bin.size()), source, sourceSize, size))
                {
                    Error(val, ix, "Failed to deserialize binary data from base64.");
                }
            }

            ParameterOffset writeHere = static_cast<char*>(allocHelper.GetShmem()->allocate(size + sizeof(Size)));

            //            std::wcout << "ABAQ   Writing (index = " << ix << ") " << size << " bytes to address " << (const void*)(writeHere.get()) << std::endl;
            //            std::wcout << "ABAQ      Location in parameter blob = " << (const void *)(m_firstFree.get()) << std::endl;
            //write the size:
            Write(*ParameterOffsetCast<Size>(writeHere), static_cast<Size>(size));

            if (bin.size() != 0)
            {
                //write the data after the size
                memcpy((writeHere+sizeof(Size)).get(), &bin[0], size);
            }
            //write the pointer in the parameter blob
            *ParameterOffsetCast<ParameterOffset>(m_firstFree) = writeHere;

            m_firstFree+=sizeof(ParameterOffset);
        }

    }

    void ParameterDatabase::Error(const DobParameter& val, const size_t valueIndex, const std::string & description)
    {
        ErrorHandler::Error("Bad Parameter Value",
                            description + "\nParameter name: " + val.m_name,
                            val.m_file,
                            val.m_values[valueIndex].m_lineNumber,
                            "dots_parameter_database");
        exit(1);
    }
}
}
}
}
