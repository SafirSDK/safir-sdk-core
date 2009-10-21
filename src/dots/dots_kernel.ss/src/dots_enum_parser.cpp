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

#include "dots_enum_parser.h"
#include <Safir/Dob/Typesystem/Internal/Id.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    EnumParser::EnumParser()
    {
        m_unit="dots_enum_parser";
    }

    void EnumParser::Reset()
    {
        m_expected=ENUM_EEL;
        m_tmpEnumeration.m_name.clear();
        m_tmpEnumeration.m_values.clear();
        m_tmpEnumVal.clear();
        m_uSummary=false;
    }

    const char* EnumParser::ValidElement(const char* e)
    {
        if (eq(e, XmlElements::ENUM)) return XmlElements::ENUM;
        if (eq(e, XmlElements::NAME)) return XmlElements::NAME;
        if (eq(e, XmlElements::VALUES)) return XmlElements::VALUES;
        if (eq(e, XmlElements::VALUE)) return XmlElements::VALUE;
        if (eq(e, XmlElements::SUMMARY)) return XmlElements::SUMMARY;
        return NULL;
    }

    bool EnumParser::StartElement(const std::string& s)
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
        if (eq(elem, XmlElements::ENUM)){
            if(m_expected!=ENUM_EEL){
                IllegalOrder(elem);
                return false;
            }
            m_expected=ENUM_NAME_EEL;
        }
        else if (eq(elem, XmlElements::NAME)){
            if(m_expected!=ENUM_NAME_EEL){
                IllegalOrder(elem);
                return false;
            }
            m_tmpEnumeration.m_name.clear();
            m_expected=VALUES_EEL;
        }
        else if (eq(elem, XmlElements::VALUES)){
            if(m_expected!=VALUES_EEL){
                IllegalOrder(elem);
                return false;
            }
            m_expected=VALUE_EEL;
        }
        else if (eq(elem, XmlElements::VALUE)){
            if(m_expected!=VALUE_EEL){
                IllegalOrder(elem);
                return false;
            }
            m_tmpEnumVal.clear();
            m_expected=VALUE_EEL;
        }

        m_elem.push(elem);
        return true;
    }

    bool EnumParser::EndElement(const std::string& s)
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

        if (eq(elem, XmlElements::ENUM)) {
            std::string checkSum=m_tmpEnumeration.m_name;
            for (size_t i=0; i<m_tmpEnumeration.m_values.size(); i++) {
                checkSum+="."+m_tmpEnumeration.m_values[i];
            }
            m_tmpEnumeration.m_checkSum=DotsId_Generate64(checkSum.c_str());
            m_tmpEnumeration.m_typeId=DotsId_Generate64(m_tmpEnumeration.m_name.c_str());
            m_result.push_back(m_tmpEnumeration);
        }
        else if (eq(elem, XmlElements::VALUE)) {
            m_tmpEnumeration.m_values.push_back(m_tmpEnumVal);
        }

        m_elem.pop();
        return true;
    }

    bool EnumParser::Content(const std::string& str)
    {
        //comments, dont care about content
        if (m_uSummary)
        {
            return true;
        }

        //real data
        if (m_elem.top()==XmlElements::NAME) {
            m_tmpEnumeration.m_name+=str;
        }
        else if (m_elem.top()==XmlElements::VALUE) {
            m_tmpEnumVal+=str;
        }

        return true;
    }

    int EnumParser::GetIndex(TypeId t)
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
