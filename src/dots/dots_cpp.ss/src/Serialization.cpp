/******************************************************************************
*
* Copyright Saab AB, 2006-2013 (http://safirsdkcore.com)
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

#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Dob/Typesystem/Operations.h>
#include <Safir/Dob/Typesystem/Utilities.h>
#include <Safir/Dob/Typesystem/Object.h>
#include <Safir/Dob/Typesystem/ObjectFactory.h>
#include <Safir/Dob/Typesystem/Internal/Kernel.h>
#include <Safir/Dob/Typesystem/Internal/InternalOperations.h>
#include <Safir/Dob/Typesystem/Exceptions.h>


namespace Safir
{
namespace Dob
{
namespace Typesystem
{   
    const std::wstring
    Serialization::ToXml(const Dob::Typesystem::ObjectPtr object)
    {
        if (object == NULL)
        {
            throw SoftwareViolationException(L"Attempt to serialize a null pointer to xml!", __WFILE__,__LINE__);
        }

        BinarySerialization bin;
        ToBinary(object, bin);
        return ToXml(bin);
    }

    const std::wstring
    Serialization::ToJson(const Dob::Typesystem::ObjectPtr object)
    {
        if (object == NULL)
        {
            throw SoftwareViolationException(L"Attempt to serialize a null pointer to json!", __WFILE__,__LINE__);
        }

        BinarySerialization bin;
        ToBinary(object, bin);
        return ToJson(bin);
    }

    const std::wstring 
    Serialization::ToXml(const BinarySerialization & bin)
    {
        return ToXml(&bin[0]);
    }

    const std::wstring
    Serialization::ToJson(const BinarySerialization & bin)
    {
        return ToJson(&bin[0]);
    }


    const std::wstring 
    Serialization::ToXml(const char * const blob)
    {
        int BUF_SIZE = 100000;
        std::vector<char> xml8(BUF_SIZE);
        Int32 resultSize;
        DotsC_BlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
        if (resultSize> BUF_SIZE)
        {
            BUF_SIZE = resultSize;
            xml8.resize(BUF_SIZE);
            DotsC_BlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
            if (resultSize != BUF_SIZE)
            {
                throw SoftwareViolationException(L"Error in serialization buffer sizes",__WFILE__,__LINE__);
            }
        }
        return Utilities::ToWstring(std::string(xml8.begin(),
                                                xml8.begin() + resultSize - 1)); //remove null.
    }

    const std::wstring
    Serialization::ToJson(const char * const blob)
    {
        int BUF_SIZE = 100000;
        std::vector<char> json8(BUF_SIZE);
        Int32 resultSize;
        DotsC_BlobToJson(&json8[0], blob, BUF_SIZE, resultSize);
        if (resultSize> BUF_SIZE)
        {
            BUF_SIZE = resultSize;
            json8.resize(BUF_SIZE);
            DotsC_BlobToJson(&json8[0], blob, BUF_SIZE, resultSize);
            if (resultSize != BUF_SIZE)
            {
                throw SoftwareViolationException(L"Error in serialization buffer sizes",__WFILE__,__LINE__);
            }
        }
        return Utilities::ToWstring(std::string(json8.begin(),
                                                json8.begin() + resultSize - 1)); //remove null.
    }

    Dob::Typesystem::ObjectPtr 
    Serialization::ToObject(const std::wstring & xml)
    {
        char * blob;
        std::string xml8 = Utilities::ToUtf8(xml);
        
        std::vector<char> xml8v(xml8.size() +1);
        xml8v.assign(xml8.begin(),xml8.end());
        xml8v.push_back(0); //null termination
        DotsC_BytePointerDeleter deleter;
        DotsC_XmlToBlob(blob, deleter, &xml8v[0]);
        if (blob == NULL)
        {
            throw IllegalValueException(L"Something is wrong with the XML-formated object", __WFILE__,__LINE__);
        }
        ObjectPtr p = ObjectFactory::Instance().CreateObject(blob);
        deleter(blob);
        return p;
    }

    Dob::Typesystem::ObjectPtr
    Serialization::ToObjectFromJson(const std::wstring & json)
    {
        char * blob;
        std::string json8 = Utilities::ToUtf8(json);

        std::vector<char> json8v(json8.size() +1);
        json8v.assign(json8.begin(),json8.end());
        json8v.push_back(0); //null termination
        DotsC_BytePointerDeleter deleter;
        DotsC_JsonToBlob(blob, deleter, &json8v[0]);
        if (blob == NULL)
        {
            throw IllegalValueException(L"Something is wrong with the XML-formated object", __WFILE__,__LINE__);
        }
        ObjectPtr p = ObjectFactory::Instance().CreateObject(blob);
        deleter(blob);
        return p;
    }

    void 
    Serialization::ToBinary(const Dob::Typesystem::ObjectPtr object, BinarySerialization & binary)
    {
        if (object == NULL)
        {
            throw SoftwareViolationException(L"Attempt to serialize a null pointer to binary!", __WFILE__,__LINE__);
        }


        DotsC_Handle handle=DotsC_CreateBlobWriter(object->GetTypeId());
        object->WriteToBlob(handle);
        DotsC_Int32 size=DotsC_CalculateBlobSize(handle);
        binary.resize(static_cast<size_t>(size));
        DotsC_WriteBlob(handle, &binary[0]);
        DotsC_DeleteBlobWriter(handle);
    }

    Dob::Typesystem::ObjectPtr 
    Serialization::ToObject(const BinarySerialization & data)
    {
        return ObjectFactory::Instance().CreateObject(&data[0]);
    }

}
}
}


