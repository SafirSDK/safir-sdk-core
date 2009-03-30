/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
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

#include "dots_property_parser.h"
#include <Safir/Dob/Typesystem/Internal/Id.h>
#include "dots_basic_types.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    PropertyParser::PropertyParser()
    {
        m_unit="dots_property_parser";
    }

    void PropertyParser::Reset()
    {
        m_expected=PROPERTY_PEL;

        m_uProperty=false;
        m_uMembers=false;
        m_uMember=false;
        m_uSummary=false;

        //Reset tmpProperty
        m_tmpProperty.m_typeId=0;
        m_tmpProperty.m_members.clear();
        m_tmpProperty.m_name.clear();
        m_tmpProperty.m_complex=false;
        //        m_tmpProperty.m_inserted=false;

        //Reset tmpMember
        m_tmpMember.m_name.clear();
        m_tmpMember.m_arrayLength=1;
        m_tmpMember.m_dataLength=0;
        m_tmpMember.m_objType=0;
        m_tmpMember.m_arrSizeFromParameter=false;
        m_tmpMember.m_strLenFromParameter=false;
    }

    const char* PropertyParser::ValidElement(const char* e)
    {
        if (eq(e, XmlElements::PROPERTY)) return XmlElements::PROPERTY;
        if (eq(e, XmlElements::NAME)) return XmlElements::NAME;
        if (eq(e, XmlElements::MEMBERS)) return XmlElements::MEMBERS;
        if (eq(e, XmlElements::MEMBER)) return XmlElements::MEMBER;
        if (eq(e, XmlElements::TYPE)) return XmlElements::TYPE;
        if (eq(e, XmlElements::ARRAY)) return XmlElements::ARRAY;
        if (eq(e, XmlElements::SUMMARY)) return XmlElements::SUMMARY;
        return NULL;
    }

    bool PropertyParser::StartElement(std::string s)
    {
        const char* elem=ValidElement(s.c_str());
        if (elem==NULL){
            IllegalElement(s.c_str());
            return false;
        }

        //comments
        if (eq(elem, XmlElements::SUMMARY))
        {
            m_uSummary=true;
            return true;
        }

        //check order of elements
        if (eq(elem, XmlElements::PROPERTY)){
            if(m_expected!=PROPERTY_PEL){
                IllegalOrder(elem);
                return false;
            }
            m_expected=PROPERTY_NAME_PEL;
            m_uProperty=true;
        }
        else if (eq(elem, XmlElements::NAME)){
            if (m_expected==PROPERTY_NAME_PEL){
                m_expected=MEMBERS_PEL;
            }
            else if (m_expected==MEMBER_NAME_PEL){
                m_expected=MEMBER_TYPE_PEL;
            }
            else {
                IllegalOrder(elem);
                return false;
            }
        }
        else if (eq(elem, XmlElements::TYPE)){
            if (m_expected==MEMBER_TYPE_PEL) {
                m_expected=MEMBER_PEL;
            }
            else {
                IllegalOrder(elem);
                return false;
            }
        }
        else if (eq(elem, XmlElements::MEMBERS)){
            if (m_expected!=MEMBERS_PEL) {
                IllegalOrder(elem);
                return false;
            }
            m_expected=MEMBER_PEL;
            m_uMembers=true;
        }
        else if (eq(elem,XmlElements::ARRAY)){
            m_tmpMember.m_propertyIsArray = true;
        }
        else if (eq(elem, XmlElements::MEMBER)){
            if (m_expected!=MEMBER_PEL) {
                IllegalOrder(elem);
                return false;
            }
            m_expected=MEMBER_NAME_PEL;
            m_uMember=true;
            m_tmpMember.m_arrayLength=1;
            m_tmpMember.m_dataLength=0;
            m_tmpMember.m_arrSizeFromParameter=false;
            m_tmpMember.m_strLenFromParameter=false;
            m_tmpMember.m_propertyIsArray = false;
        }

        m_elem.push(elem);
        return true;
    }

    bool PropertyParser::EndElement(std::string s)
    {
        const char* elem=ValidElement(s.c_str());
        if (elem==NULL){
            IllegalElement(s.c_str());
            return false;
        }
        if (eq(elem, XmlElements::SUMMARY) && m_uSummary)
        {
            m_uSummary=false;
            return true;
        }
        if (!eq(m_elem.top(), elem)) {
            ElementMismatch(elem);
            return false;
        }

        if (eq(elem, XmlElements::PROPERTY)) {
            m_result.push_back(m_tmpProperty);
            m_uProperty=false;
        }
        else if (eq(elem, XmlElements::MEMBERS)) {
            m_uMembers=false;
        }
        else if (eq(elem, XmlElements::MEMBER)) {
            m_tmpProperty.m_members.push_back(m_tmpMember);
            m_uMember=false;
        }

        m_elem.pop();
        return true;
    }

    bool PropertyParser::Content(std::string str)
    {
        //comments, dont care about content
        if (m_uSummary)
        {
            return true;
        }

        //real data
        if (m_elem.top()==XmlElements::NAME) {
            if (m_uProperty && !m_uMember){
                m_tmpProperty.m_name=str;
                m_tmpProperty.m_typeId=DotsId_Generate64(m_tmpProperty.m_name.c_str());
            }
            else if (m_uMember){
                m_tmpMember.m_name=str;
            }
        }
        else if (m_elem.top()==XmlElements::TYPE) {
            if (m_uMember) {
                m_tmpMember.m_type=Safir::Dob::Typesystem::Internal::BasicTypes::MemberTypeOf(str.c_str());
                if (m_tmpMember.m_type==ObjectMemberType) {
                    m_tmpMember.m_objType=DotsId_Generate64(str.c_str());
                    m_tmpProperty.m_complex=true;
                }
            }
        }

        return true;
    }

    int PropertyParser::GetIndex(TypeId t)
    {
        for (size_t i=0; i<m_result.size(); i++)
        {
            if (m_result[i].m_typeId==t)
                return static_cast<int>(i);
        }
        return -1;
    }
}
}
}
}
