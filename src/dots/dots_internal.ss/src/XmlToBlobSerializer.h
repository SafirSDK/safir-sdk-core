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
#ifndef __DOTS_INTERNAL_XML_TO_BLOB_H__
#define __DOTS_INTERNAL_XML_TO_BLOB_H__

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
    class XmlToBlobSerializer : private boost::noncopyable
    {
    public:
        XmlToBlobSerializer(const TypeRepository* repository, const char* xml);
        void operator()(std::vector<char>& blob) const;

    private:
        const TypeRepository* m_repository;
        size_t m_xmlSize;
        boost::property_tree::ptree m_pt;
        const BlobLayoutImpl m_blobLayout;

        void SerializeMembers(const std::string& typeName, std::vector<char>& blob, const boost::property_tree::ptree& members) const;

        void SetMember(const MemberDescription* md,
                       DotsC_MemberIndex memIx,
                       DotsC_ArrayIndex arrIx,
                       const boost::property_tree::ptree& memberContent,
                       std::vector<char>& blob,
                       char* &beginningOfUnused) const;

        DotsC_TypeId StringToTypeId(const std::string& str) const;
        std::pair<DotsC_TypeId, const char*> StringToHash(const std::string& str) const;

        void CreateSpaceForDynamicMember(std::vector<char>& blob,
                                         char* & beginningOfUnused,
                                         size_t dynamicMemberSizeNeeded) const;
    };

}
}
}
} //end namespace Safir::Dob::Typesystem::Internal

#endif
