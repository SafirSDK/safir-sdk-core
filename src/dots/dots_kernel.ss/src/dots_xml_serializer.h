/******************************************************************************
*
* Copyright Saab AB, 2004-2008 (http://www.safirsdk.com)
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

#ifndef _dots_xml_serializer_h
#define _dots_xml_serializer_h

#include "dots_internal_defs.h"
#include "dots_member_description.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Functionality for serialization of objects in binary format into xml format.
     */
    class BlobToXmlSerializer:
        private boost::noncopyable
    {
    public:
        BlobToXmlSerializer(){}
        std::string Serialize(const char * const blobSource);

    private:
        std::string xml;

        //this is to be able to check that we don't pass the end of the blob while
        //reading it. Just so that we can detect broken blobs.
        const char * m_blobEnd;

        //specific
        void WriteObject(const char * const, const bool includeNamespace = false);
        void WriteTypeId(const TypeId tid);
        //void WriteEntityId(const DotsC_EntityId& oid);
        void WriteError(const char* msg);

        //members - returns true if not null or changed to null. I.e should be part of the serialization
        bool WriteEnumMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteBooleanMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteInt32Member(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteInt64Member(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteFloat32Member(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteFloat64Member(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteTypeIdMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteHashedIdMember(const MemberDescription * memberDesc, const char * const blob, MemberIndex member, int ix);
        bool WriteEntityIdMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteStringMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteObjectMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);
        bool WriteBinaryMember(const MemberDescription * memberDesc, const char * const, MemberIndex member, int ix);

        //general
        void WriteElement(const char* element, const char* content);
        void WriteStartElement(const char* element);
        void WriteEndElement(const char* element);

        void InsertInt(const Int32 t);
        void InsertDouble(const double t);
        void InsertFloat(const float t);
        void InsertI64(const Int64 t);
        void InsertUI64(const UInt64 t);
        void InsertBool(const bool b);

    };
}
}
}
}
#endif
