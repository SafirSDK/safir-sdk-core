/******************************************************************************
*
* Copyright Saab AB, 2004-2014 (http://safir.sourceforge.net)
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
#include <Safir/Dob/Typesystem/ToolSupport/Internal/Blob.h>
#include "AnyObject.pb.h"

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
    Blob::Blob(boost::int64_t typeId)
        :m_blobSize(0)
        ,m_typeId(typeId)
        ,m_object(new AnyObject())
        ,m_currentObjectMemberIndex(-1)
    {
    }

    Blob::Blob(const char* blob)
        :m_blobSize(Blob::GetSize(blob))
        ,m_typeId(Blob::GetTypeId(blob))
        ,m_object(new AnyObject())
        ,m_currentObjectMemberIndex(-1)
    {
        bool ok=m_object->ParseFromArray(static_cast<const void*>(blob+HeaderSize), m_blobSize-HeaderSize);
        if (!ok)
        {
            std::ostringstream os;
            os<<"Failed to deserialize blob. TypeId: "<<m_typeId<<", size: "<<m_blobSize<<". (ProtoBuf.ParseFromArray failed)";
            throw ParseError("Bad blob", os.str(), "Blob.cpp", 2000);
        }
    }

    boost::int32_t Blob::GetSize(const char *blob)
    {
        boost::int32_t val;
        memcpy(&val, blob, sizeof(boost::int32_t));
        return val;
    }

    boost::int64_t Blob::GetTypeId(const char *blob)
    {
        boost::int64_t val;
        memcpy(&val, blob+sizeof(boost::int32_t), sizeof(boost::int32_t));
        return val;

    }

    boost::int32_t Blob::CalculateBlobSize()
    {
        m_blobSize=static_cast<boost::int32_t>(HeaderSize)+static_cast<boost::int32_t>(m_object->ByteSize());
        return m_blobSize;
    }

    void Blob::Serialize(char* destBlob)
    {
        //set size, header+protobufSize
        m_blobSize=static_cast<boost::int32_t>(HeaderSize)+static_cast<boost::int32_t>(m_object->GetCachedSize());

        //copy header(size, typeId), not part of the protbuf type
        memcpy(destBlob, reinterpret_cast<void*>(&m_blobSize), sizeof(m_blobSize));
        memcpy(destBlob+sizeof(m_blobSize), reinterpret_cast<void*>(&m_typeId), sizeof(m_typeId));

        //serialize the protobuf content immediately after the header in destBuf
        google::protobuf::uint8* content=reinterpret_cast<google::protobuf::uint8*>(destBlob+HeaderSize);
        m_object->SerializeWithCachedSizesToArray(content);
    }

    bool Blob::MoveToMember(boost::int32_t memberIndex)
    {
        if (m_currentObjectMemberIndex>=0 && m_object->members(m_currentObjectMemberIndex).member_id()==memberIndex)
        {
            return true; //already there
        }

        for (int i=0; i<m_object->members_size(); ++i)
        {
            if (m_object->members(i).member_id()==memberIndex)
            {
                m_currentObjectMemberIndex=i;
                return true;
            }
        }

        return false;
    }

    bool Blob::IsChangedHere() const
    {
        if (m_currentObjectMemberIndex<0)
        {
            return false;
        }

        return  m_object->members(m_currentObjectMemberIndex).has_is_changed_here() &&
                m_object->members(m_currentObjectMemberIndex).is_changed_here();

    }

    int Blob::NumberOfValues() const
    {
        if (m_currentObjectMemberIndex<0)
        {
            return 0;
        }

        return m_object->members(m_currentObjectMemberIndex).values_size();
    }

    bool Blob::IsNull(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        bool hasValue=
                val.value().has_int32_value() ||
                val.value().has_int64_value() ||
                val.value().has_float32_value() ||
                val.value().has_float64_value() ||
                val.value().has_boolean_value() ||
                val.value().has_string_value() ||
                val.value().has_binary_value() ||
                val.value().has_hash_value();

        return !hasValue;
    }

    bool Blob::IsChanged(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return (val.has_is_changed() && val.is_changed());
    }

    boost::int32_t Blob::GetKeyInt32(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.key().int32_value();
    }

    boost::int64_t Blob::GetKeyInt64(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.key().int64_value();
    }

    boost::int64_t Blob::GetKeyHash(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.key().hash_value();
    }

    const std::string& Blob::GetKeyString(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.key().string_value();
    }

    boost::int32_t Blob::GetValueInt32(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().int32_value();
    }

    boost::int64_t Blob::GetValueInt64(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().int64_value();
    }

    float Blob::GetValueFloat32(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().float32_value();
    }

    double Blob::GetValueFloat64(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().float64_value();
    }

    bool Blob::GetValueBool(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().boolean_value();
    }

    boost::int64_t Blob::GetValueHash(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().hash_value();
    }

    const std::string& Blob::GetValueString(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        return val.value().string_value();
    }

    std::pair<const char*, boost::int32_t> Blob::GetValueBinary(int index) const
    {
        const AnyObject_Value& val=m_object->members(m_currentObjectMemberIndex).values(index);
        const std::string& s=val.value().binary_value();
        return std::make_pair(s.c_str(), static_cast<boost::int32_t>(s.size()));
    }

    void Blob::AddMember(boost::int32_t memberIndex)
    {
        AnyObject_Member* m=m_object->add_members();

        m_currentObjectMemberIndex=m_object->members_size()-1;
    }

    void Blob::SetChangedHere(bool isChanged)
    {
        m_object->mutable_members(m_currentObjectMemberIndex)->set_is_changed_here(isChanged);
    }

    void Blob::AddValue(bool isChanged)
    {
        AnyObject_Value* value=m_object->mutable_members(m_currentObjectMemberIndex)->add_values();
        if (isChanged)
        {
            value->set_is_changed(isChanged);
        }
    }

    void Blob::SetKeyInt32(boost::int32_t val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_key()->set_int32_value(val);
    }

    void Blob::SetKeyInt64(boost::int64_t val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_key()->set_int64_value(val);
    }

    void Blob::SetKeyHash(boost::int64_t val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_key()->set_hash_value(val);
    }

    void Blob::SetKeyString(const std::string &val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_key()->set_string_value(val);
    }

    void Blob::SetValueInt32(boost::int32_t val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_int32_value(val);
    }

    void Blob::SetValueInt64(boost::int64_t val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_int64_value(val);
    }

    void Blob::SetValueFloat32(float val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_float32_value(val);
    }

    void Blob::SetValueFloat64(double val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_float64_value(val);
    }

    void Blob::SetValueBool(bool val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_boolean_value(val);
    }

    void Blob::SetValueHash(boost::int64_t val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_hash_value(val);
    }

    void Blob::SetValueString(const std::string &val)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_string_value(val);
    }

    void Blob::SetValueBinary(const char *val, boost::int32_t size)
    {
        AnyObject_Member* m=m_object->mutable_members(m_currentObjectMemberIndex);
        AnyObject_Value* v=m->mutable_values(m->values_size()-1);
        v->mutable_value()->set_binary_value(val, static_cast<size_t>(size));
    }
}
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport::Internal
