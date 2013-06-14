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
#include <stdexcept>
#include <boost/property_tree/xml_parser.hpp>
#include <boost/property_tree/json_parser.hpp>
#include "BlobToJsonSerializer.h"
#include "BasicTypes.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{

//Namepspace boostfix is a hack that prevents boosts json_writer to make all values to a strings.
//Instead nothing will be quoted and strings must manually be quoted before insertion in the property-tree
//All code are copied from the boost file 'json_parser_write.hpp'. Modified lines are commented.
namespace boostfix
{
    using namespace boost;
    using namespace boost::detail;
    using namespace boost::details;
    using namespace boost::property_tree;
    using namespace boost::property_tree::json_parser;

    // Create necessary escape sequences from illegal characters
    template<class Ch>
    std::basic_string<Ch> create_escapes(const std::basic_string<Ch> &s)
    {
        std::basic_string<Ch> result;
        typename std::basic_string<Ch>::const_iterator b = s.begin();
        typename std::basic_string<Ch>::const_iterator e = s.end();
        while (b != e)
        {
            // This assumes an ASCII superset. But so does everything in PTree.
            // We escape everything outside ASCII, because this code can't
            // handle high unicode characters.
            if (*b == 0x20 || *b == 0x21 || (*b >= 0x23 && *b <= 0x2E) ||
                (*b >= 0x30 && *b <= 0x5B) || (*b >= 0x5D && *b <= 0xFF))
                result += *b;
            else if (*b == Ch('\b')) result += Ch('\\'), result += Ch('b');
            else if (*b == Ch('\f')) result += Ch('\\'), result += Ch('f');
            else if (*b == Ch('\n')) result += Ch('\\'), result += Ch('n');
            else if (*b == Ch('\r')) result += Ch('\\'), result += Ch('r');
            else if (*b == Ch('/')) result += Ch('\\'), result += Ch('/');
            else if (*b == Ch('"'))  result += Ch('"'); //Modified by JOOT!
            else if (*b == Ch('\\')) result += Ch('\\'), result += Ch('\\');
            else
            {
                const char *hexdigits = "0123456789ABCDEF";
                typedef typename make_unsigned<Ch>::type UCh;
                unsigned long u = (std::min)(static_cast<unsigned long>(
                                                 static_cast<UCh>(*b)),
                                             0xFFFFul);
                int d1 = u / 4096; u -= d1 * 4096;
                int d2 = u / 256; u -= d2 * 256;
                int d3 = u / 16; u -= d3 * 16;
                int d4 = u;
                result += Ch('\\'); result += Ch('u');
                result += Ch(hexdigits[d1]); result += Ch(hexdigits[d2]);
                result += Ch(hexdigits[d3]); result += Ch(hexdigits[d4]);
            }
            ++b;
        }
        return result;
    }

    template<class Ptree>
    void write_json_helper(std::basic_ostream<typename Ptree::key_type::value_type> &stream,
                           const Ptree &pt,
                           int indent, bool pretty)
    {

        typedef typename Ptree::key_type::value_type Ch;
        typedef typename std::basic_string<Ch> Str;

        // Value or object or array
        if (indent > 0 && pt.empty())
        {
            // Write value
            Str data = create_escapes(pt.template get_value<Str>());
            stream << data; //Modified by JOOT!

        }
        else if (indent > 0 && pt.count(Str()) == pt.size())
        {
            // Write array
            stream << Ch('[');
            if (pretty) stream << Ch('\n');
            typename Ptree::const_iterator it = pt.begin();
            for (; it != pt.end(); ++it)
            {
                if (pretty) stream << Str(4 * (indent + 1), Ch(' '));
                write_json_helper(stream, it->second, indent + 1, pretty);
                if (boost::next(it) != pt.end())
                    stream << Ch(',');
                if (pretty) stream << Ch('\n');
            }
            stream << Str(4 * indent, Ch(' ')) << Ch(']');

        }
        else
        {
            // Write object
            stream << Ch('{');
            if (pretty) stream << Ch('\n');
            typename Ptree::const_iterator it = pt.begin();
            for (; it != pt.end(); ++it)
            {
                if (pretty) stream << Str(4 * (indent + 1), Ch(' '));
                stream << Ch('"') << create_escapes(it->first) << Ch('"') << Ch(':');
                if (pretty) {
                    if (it->second.empty())
                        stream << Ch(' ');
                    else
                        stream << Ch('\n') << Str(4 * (indent + 1), Ch(' '));
                }
                write_json_helper(stream, it->second, indent + 1, pretty);
                if (boost::next(it) != pt.end())
                    stream << Ch(',');
                if (pretty) stream << Ch('\n');
            }
            if (pretty) stream << Str(4 * indent, Ch(' '));
            stream << Ch('}');
        }
    }

    template<class Ptree>
    void write_json_internal(std::basic_ostream<typename Ptree::key_type::value_type> &stream,
                             const Ptree &pt,
                             const std::string &filename,
                             bool pretty)
    {
        if (!verify_json(pt, 0))
            BOOST_PROPERTY_TREE_THROW(json_parser_error("ptree contains data that cannot be represented in JSON format", filename, 0));
        Safir::Dob::Typesystem::Internal::boostfix::write_json_helper(stream, pt, 0, pretty); //Modified by JOOT!
        stream << std::endl;
        if (!stream.good())
            BOOST_PROPERTY_TREE_THROW(json_parser_error("write error", filename, 0));
    }
}

    BlobToJsonSerializer::BlobToJsonSerializer(const TypeRepository* repository, const char* blob)
        :m_repository(repository)
        ,m_blob(blob)
        ,m_blobLayout(repository)
    {
    }

    void BlobToJsonSerializer::operator()(std::ostream& os) const
    {
        boost::property_tree::ptree content;
        SerializeMembers(content);
        boost::property_tree::ptree root;
        root.push_back(std::make_pair(GetClass()->GetName(), content));
        boostfix::write_json_internal(os, root, std::string(), true);
        //boost::property_tree::json_parser::write_json(os, root);
    }

    void BlobToJsonSerializer::SerializeMembers(boost::property_tree::ptree& content) const
    {
        const ClassDescription* cd=GetClass();

        content.add("_DobType", Quoted(cd->GetName()));

        for (DotsC_MemberIndex memberIx=0; memberIx<cd->GetNumberOfMembers(); ++memberIx)
        {
            const MemberDescription* md=cd->GetMember(memberIx);
            if (!md->IsArray()) //normal member
            {
                SerializeMember(md, memberIx, 0, md->GetName(), content);
            }
            else //array member
            {
                bool nonNullValueInserted=false;
                boost::property_tree::ptree arrayValues;
                for (DotsC_ArrayIndex arrIx=0; arrIx<md->GetArraySize(); ++arrIx)
                {
                    if (SerializeMember(md, memberIx, arrIx, "", arrayValues))
                    {
                        nonNullValueInserted=true;
                    }
                    else
                    {
                        //element at index is null, then we must insert null value
                        arrayValues.push_back(std::make_pair("", "null"));
                    }
                }
                if (nonNullValueInserted) //only add array element if there are non-null values
                {
                    content.add_child(md->GetName(), arrayValues);
                }
            }
        }
    }

    bool BlobToJsonSerializer::SerializeMember(const MemberDescription* md,
                                               DotsC_MemberIndex memberIndex,
                                               DotsC_ArrayIndex arrayIndex,
                                               const char* elementName,
                                               boost::property_tree::ptree& pt) const
    {
        switch(md->GetMemberType())
        {
        case BooleanMemberType:
        {
            bool val=true;
            DotsC_MemberStatus status=m_blobLayout.GetMember<bool>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                pt.push_back(std::make_pair(elementName, val ? "true" : "false"));
                return true;
            }            
        }
            break;

        case EnumerationMemberType:
        {
            DotsC_Int32 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int32>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                const char* enumVal=m_repository->GetEnum(md->GetTypeId())->GetValueName(val);
                pt.push_back(std::make_pair(elementName, enumVal));
                return true;
            }
        }
            break;

        case Int32MemberType:
        {
            DotsC_Int32 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int32>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                pt.push_back(std::make_pair(elementName, boost::lexical_cast<std::string>(val)));
                return true;
            }
        }
            break;

        case Int64MemberType:
        {
            DotsC_Int64 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int64>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                pt.push_back(std::make_pair(elementName, boost::lexical_cast<std::string>(val)));
                return true;
            }
        }
            break;

        case TypeIdMemberType:
        {
            DotsC_Int64 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Int64>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                const char* typeName=TypeIdToString(val);
                if (typeName)
                {
                    pt.push_back(std::make_pair(elementName, Quoted(typeName)));
                }
                else
                {
                    pt.push_back(std::make_pair(elementName, boost::lexical_cast<std::string>(val)));
                }
                return true;
            }
        }
            break;

        case InstanceIdMemberType:
        case ChannelIdMemberType:
        case HandlerIdMemberType:
        {
            DotsC_Int64 val=0;
            const char* hashStr=NULL;
            DotsC_MemberStatus status=m_blobLayout.GetMemberWithOptionalString(m_blob, memberIndex, arrayIndex, val, hashStr);
            if (!status.IsNull())
            {
                if (hashStr)
                {
                    pt.push_back(std::make_pair(elementName, Quoted(hashStr)));
                }
                else
                {
                    pt.push_back(std::make_pair(elementName, boost::lexical_cast<std::string>(val)));
                }
                return true;
            }
        }
            break;

        case EntityIdMemberType:
        {
            DotsC_EntityId entId;
            const char* hashStr=0;
            DotsC_MemberStatus status=m_blobLayout.GetMemberWithOptionalString(m_blob, memberIndex, arrayIndex, entId, hashStr);
            if (!status.IsNull())
            {
                boost::property_tree::ptree entIdPt;
                const char* typeName=TypeIdToString(entId.typeId);
                if (typeName)
                {
                    entIdPt.add("name", Quoted(typeName));
                }
                else
                {
                    entIdPt.add("name", entId.typeId);
                }

                if (hashStr)
                {
                    entIdPt.add("instanceId", Quoted(hashStr));
                }
                else
                {
                    entIdPt.add("instanceId", entId.instanceId);
                }
                pt.push_back(std::make_pair(elementName, entIdPt));
                return true;
            }
        }
            break;

        case StringMemberType:
        {
            const char* strVal=NULL;
            DotsC_Int32 size=0;
            DotsC_MemberStatus status=m_blobLayout.GetDynamicMember(m_blob, memberIndex, arrayIndex, strVal, size);
            if (!status.IsNull())
            {
                pt.push_back(std::make_pair(elementName, Quoted(strVal)));
                return true;
            }
        }
            break;

        case ObjectMemberType:
        {
            const char* obj=NULL;
            DotsC_Int32 size=0;
            DotsC_MemberStatus status=m_blobLayout.GetDynamicMember(m_blob, memberIndex, arrayIndex, obj, size);
            if (!status.IsNull())
            {
                boost::property_tree::ptree members; //Serialize without the root-element, only members
                BlobToJsonSerializer objParser(m_repository, obj);
                objParser.SerializeMembers(members);
                pt.push_back(std::make_pair(elementName, members));
                return true;
            }
        }
            break;

        case BinaryMemberType:
        {
            const char* binary=NULL;
            DotsC_Int32 size=0;
            DotsC_MemberStatus status=m_blobLayout.GetDynamicMember(m_blob, memberIndex, arrayIndex, binary, size);
            if (!status.IsNull())
            {
                std::string bin(binary, size);
                pt.push_back(std::make_pair(elementName, Quoted(BasicTypes::ToBase64(bin))));
                return true;
            }
        }
            break;

            //  32 bit floats
        case Float32MemberType:
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
        {
            DotsC_Float32 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Float32>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                pt.push_back(std::make_pair(elementName, classic_string_cast<std::string>(val)));
                return true;
            }
        }
            break;

            //  64 bit floats
        case Float64MemberType:
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
        {
            DotsC_Float64 val=0;
            DotsC_MemberStatus status=m_blobLayout.GetMember<DotsC_Float64>(m_blob, memberIndex, arrayIndex, val);
            if (!status.IsNull())
            {
                pt.push_back(std::make_pair(elementName, classic_string_cast<std::string>(val)));
                return true;
            }
        }
            break;
        }

        return false;
    }

    const char* BlobToJsonSerializer::TypeIdToString(DotsC_TypeId tid) const
    {
        const ClassDescription* cd=m_repository->GetClass(tid);
        if (cd)
        {
            return cd->GetName();
        }

        const EnumDescription* ed=m_repository->GetEnum(tid);
        if (ed)
        {
            return ed->GetName();
        }

        const PropertyDescription* pd=m_repository->GetProperty(tid);
        if (pd)
        {
            return pd->GetName();
        }

        return NULL;
    }

    const ClassDescription* BlobToJsonSerializer::GetClass() const
    {
        TypeId typeId=m_blobLayout.GetTypeId(m_blob);
        const ClassDescription* cd=m_repository->GetClass(typeId);
        if (cd==NULL)
        {
            std::ostringstream os;
            os<<"Corrupt blob. Can't find type descriptor for blob with typeId="<<typeId;
            throw ParseError("Binary to JSON error", os.str(), "", 300);
        }
        return cd;
    }

}
}
}
} //end namespace Safir::Dob::Typesystem::Internal
