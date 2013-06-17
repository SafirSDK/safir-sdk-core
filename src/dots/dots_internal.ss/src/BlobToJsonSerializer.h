/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://www.safirsdk.com)
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
#ifndef __DOTS_INTERNAL_BLOB_TO_JSON_H__
#define __DOTS_INTERNAL_BLOB_TO_JSON_H__

#include <string>
#include <vector>
#include <sstream>
#include <boost/noncopyable.hpp>
#include <boost/property_tree/ptree.hpp>
#include <Safir/Dob/Typesystem/Internal/TypeRepository.h>
#include "BlobLayoutImpl.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    class BlobToJsonSerializer : private boost::noncopyable
    {
    public:
        BlobToJsonSerializer(const TypeRepository* repository);
        void operator()(const char* blob, std::ostream& os) const;

    private:
        const TypeRepository* m_repository;
        const BlobLayoutImpl m_blobLayout;

        void SerializeMembers(const char* blob, boost::property_tree::ptree& content) const;

        bool SerializeMember(const char* blob,
                             const MemberDescription* md,
                             DotsC_MemberIndex memberIndex,
                             DotsC_ArrayIndex arrayIndex,
                             const char* elementName,
                             boost::property_tree::ptree& pt) const;

        const char* TypeIdToString(DotsC_TypeId tid) const;
        const ClassDescription* GetClass(const char* blob) const;
        static std::string Quoted(const std::string& str)
        {
            std::ostringstream os;
            os<<'\"'<<str<<'\"';
            return os.str();
        }
    };

}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
