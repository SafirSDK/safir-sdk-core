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
        XmlToBlobSerializer(const TypeRepository* repository);
        void operator()(const char* xml, std::vector<char>& blob) const;
        void operator()(const boost::property_tree::ptree& xml, std::vector<char>& blob) const;

        //This one is for internal use and should be considered private. It parses an xml-object but starts one level in from the root
        //i.e <myObject type="anyType><myInt>4</myInt><myString>hello</myString></myObject>, when using this mehtod the ptree members
        // must be <myInt>4</myInt><myString>hello</myString> and typeName must be handed as in-parameter. In then normal case when we
        //have an ptree at root level use operator()(const boost::property_tree::ptree& xml, std::vector<char>& blob)
        void SerializeObjectContent(const std::string& typeName, std::vector<char>& blob, const boost::property_tree::ptree& members) const;

    private:
        const TypeRepository* m_repository;
        const BlobLayoutImpl m_blobLayout;

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
