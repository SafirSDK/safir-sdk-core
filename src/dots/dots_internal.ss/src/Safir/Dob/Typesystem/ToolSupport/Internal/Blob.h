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
#ifndef __DOTS_INTERNAL_BLOB_H__
#define __DOTS_INTERNAL_BLOB_H__

#include <boost/cstdint.hpp>
#include <boost/shared_ptr.hpp>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>

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
    class AnyObject; //forward declaration of protoBuf type

    /**
     * Wrapper around a protobub AnyObject. No checks made here that a type is correct according to dou-files
     */
    class DOTS_INTERNAL_API Blob
    {
    public:
        static boost::int32_t GetSize(const char* blob);
        static boost::int64_t GetTypeId(const char* blob);

        //create new empty blob of specific type
        Blob(boost::int64_t typeId);

        //create blob object by deserialize a raw blob
        Blob(const char* blob);

        //calculate the blob size
        boost::int32_t CalculateBlobSize();

        //get known size of blob, does not calculate
        boost::int32_t Size() const {return m_blobSize;}

        //get typeId of blob
        boost::int64_t TypeId() const {return m_typeId;}

        //serialize current content to destBlob
        void Serialize(char* destBlob);

        //----------------------------------------
        // Read
        //----------------------------------------
        //move to specific member, returns true if member was found
        bool MoveToMember(boost::int32_t memberIndex);

        //changed at member top level, only meaningful for containers and objects
        bool IsChangedHere() const;

        //get number of values in current member
        int NumberOfValues() const;

        //is value null
        bool IsNull(int index) const;

        //is value changed
        bool IsChanged(int index) const;

        //get keys, undefined behaviour if IsNull=true
        boost::int32_t GetKeyInt32(int index) const;
        boost::int64_t GetKeyInt64(int index) const;
        boost::int64_t GetKeyHash(int index) const;
        const std::string& GetKeyString(int index) const;

        //get value
        boost::int32_t GetValueInt32(int index) const;
        boost::int64_t GetValueInt64(int index) const;
        float GetValueFloat32(int index) const;
        double GetValueFloat64(int index) const;
        bool GetValueBool(int index) const;
        boost::int64_t GetValueHash(int index) const;
        const std::string& GetValueString(int index) const;
        std::pair<const char*, boost::int32_t> GetValueBinary(int index) const;

        //----------------------------------------
        // Write
        //----------------------------------------
        //add member and move to the new member, no check for duplicates
        void AddMember(boost::int32_t memberIndex);

        //set isChanged on top level, only meaningful for containers and objects
        void SetChangedHere(bool isChanged);

        //append a new value to current member and set the changeFlag and isNull=true
        void AddValue(bool isChanged);

        //set key for last added value
        void SetKeyInt32(boost::int32_t val);
        void SetKeyInt64(boost::int64_t val);
        void SetKeyHash(boost::int64_t val);
        void SetKeyString(const std::string& val);

        //set value for last added value, after a value have been set IsNull=false
        void SetValueInt32(boost::int32_t val);
        void SetValueInt64(boost::int64_t val);
        void SetValueFloat32(float val);
        void SetValueFloat64(double val);
        void SetValueBool(bool val);
        void SetValueHash(boost::int64_t val);
        void SetValueString(const std::string& val);
        void SetValueBinary(const char* val, boost::int32_t size);


    private:
        static const size_t HeaderSize=sizeof(boost::int32_t)+sizeof(boost::int64_t);

        boost::int32_t m_blobSize;
        boost::int64_t m_typeId;
        boost::shared_ptr<AnyObject> m_object;
        int m_currentObjectMemberIndex;
    };

}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
