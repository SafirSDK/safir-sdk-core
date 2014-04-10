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

        //create new empty blob of specific type and initiates all members with null and unchanged
        Blob(boost::int64_t typeId, int numberOfMembers);

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
        //changed at member top level
        bool IsChangedTopLevel(boost::int32_t member) const;

        //get number of values in current member
        int NumberOfValues(boost::int32_t member) const;

        //is value null
        bool IsNull(boost::int32_t member, int index) const;

        //is value changed
        bool IsChanged(boost::int32_t member, int index) const;

        //get keys, undefined behaviour if IsNull=true
        boost::int32_t GetKeyInt32(boost::int32_t member, int index) const;
        boost::int64_t GetKeyInt64(boost::int32_t member, int index) const;
        boost::int64_t GetKeyHash(boost::int32_t member, int index) const;
        const std::string& GetKeyString(boost::int32_t member, int index) const;

        //get value
        boost::int32_t GetValueInt32(boost::int32_t member, int index) const;
        boost::int64_t GetValueInt64(boost::int32_t member, int index) const;
        float GetValueFloat32(boost::int32_t member, int index) const;
        double GetValueFloat64(boost::int32_t member, int index) const;
        bool GetValueBool(boost::int32_t member, int index) const;
        boost::int64_t GetValueHash(boost::int32_t member, int index) const;
        const std::string& GetValueString(boost::int32_t member, int index) const;
        std::pair<const char*, boost::int32_t> GetValueBinary(boost::int32_t member, int index) const;

        //----------------------------------------
        // Write
        //----------------------------------------        
        //set isChanged on top level, only meaningful for containers and objects
        void SetChangedTopLevel(boost::int32_t member, bool isChanged);

        //append a new value to current member and set the changeFlag and isNull=true
        //returns the index of the inserted value.
        int AddValue(boost::int32_t member, bool isChanged);

        //set change flat on individual value
        void SetChanged(boost::int32_t member, int index, bool isChanged);

        //set key for last added value
        void SetKeyInt32(boost::int32_t member, int index, boost::int32_t val);
        void SetKeyInt64(boost::int32_t member, int index, boost::int64_t val);
        void SetKeyHash(boost::int32_t member, int index, boost::int64_t val);
        void SetKeyString(boost::int32_t member, int index, const std::string& val);

        //set value for last added value, after a value have been set IsNull=false
        void SetValueInt32(boost::int32_t member, int index, boost::int32_t val);
        void SetValueInt64(boost::int32_t member, int index, boost::int64_t val);
        void SetValueFloat32(boost::int32_t member, int index, float val);
        void SetValueFloat64(boost::int32_t member, int index, double val);
        void SetValueBool(boost::int32_t member, int index, bool val);
        void SetValueHash(boost::int32_t member, int index, boost::int64_t val);
        void SetValueString(boost::int32_t member, int index, const std::string& val);
        void SetValueBinary(boost::int32_t member, int index, const char* val, boost::int32_t size);

    private:
        static const size_t HeaderSize=sizeof(boost::int32_t)+sizeof(boost::int64_t);

        boost::int32_t m_blobSize;
        boost::int64_t m_typeId;
        boost::shared_ptr<AnyObject> m_object;
    };

}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
