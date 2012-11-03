/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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

        int BUF_SIZE = 100000;
        std::vector<char> xml8(BUF_SIZE);
        Int32 resultSize;
        DotsC_BetterBlobToXml(&xml8[0], &bin[0], BUF_SIZE, resultSize);
        if (resultSize> BUF_SIZE)
        {
            BUF_SIZE = resultSize;
            xml8.resize(BUF_SIZE);
            DotsC_BetterBlobToXml(&xml8[0], &bin[0], BUF_SIZE, resultSize);
            if (resultSize != BUF_SIZE)
            {
                throw SoftwareViolationException(L"Error in serialization buffer sizes",__WFILE__,__LINE__);
            }
        }
        return Utilities::ToWstring(std::string(xml8.begin(),xml8.begin() + resultSize));
    }

    const std::wstring 
    Serialization::ToXml(const BinarySerialization & bin)
    {
        return ToXml(&bin[0]);
    }

    const std::wstring 
    Serialization::ToXml(const char * const blob)
    {
        int BUF_SIZE = 100000;
        std::vector<char> xml8(BUF_SIZE);
        Int32 resultSize;
        DotsC_BetterBlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
        if (resultSize> BUF_SIZE)
        {
            BUF_SIZE = resultSize;
            xml8.resize(BUF_SIZE);
            DotsC_BetterBlobToXml(&xml8[0], blob, BUF_SIZE, resultSize);
            if (resultSize != BUF_SIZE)
            {
                throw SoftwareViolationException(L"Error in serialization buffer sizes",__WFILE__,__LINE__);
            }
        }
        return Utilities::ToWstring(std::string(xml8.begin(),xml8.begin() + resultSize));
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


    void 
    Serialization::ToBinary(const Dob::Typesystem::ObjectPtr object, BinarySerialization & binary)
    {
        if (object == NULL)
        {
            throw SoftwareViolationException(L"Attempt to serialize a null pointer to binary!", __WFILE__,__LINE__);
        }

        const Safir::Dob::Typesystem::Int32 blobSize = object->CalculateBlobSize();
        binary.resize(blobSize);
        
        char * beginningOfUnused = 0;
        DotsC_FormatBlob(&binary[0], blobSize, object->GetTypeId(),beginningOfUnused);
        object->WriteToBlob(&binary[0], beginningOfUnused);
        if (beginningOfUnused != &binary[0] + blobSize)
        {
            std::wcout << "ToBinary: unexpected blob size mismatch! overwrite by " << static_cast<int>(beginningOfUnused - &binary[0] - blobSize) << std::endl;
        }
    }

    Dob::Typesystem::ObjectPtr 
    Serialization::ToObject(const BinarySerialization & data)
    {
        return ObjectFactory::Instance().CreateObject(&data[0]);
    }

}
}
}


