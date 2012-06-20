/******************************************************************************
*
* Copyright Saab AB, 2004-2012 (http://www.safirsdk.com)
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
#ifndef __DOTS_ELEMENT_PARSER_BASE_H__
#define __DOTS_ELEMENT_PARSER_BASE_H__

#include <iostream>
#include <string>
#include <vector>
#include <stack>
#include <assert.h>
#include <algorithm>
#include <boost/bind.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/vector.hpp>
#include "ParseAlgorithms.h"
#include "OccurrenceRules.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Parser
{
    //------------------------------------------------------------
    //Abstract base class for all parsers.
    //------------------------------------------------------------
    class ElementParserBase
    {
    public:
        explicit ElementParserBase() : m_parent(NULL) {}
        explicit ElementParserBase(const ElementParserBase* parent) : m_parent(parent) {}
        virtual bool Match(const std::string& name, ParseState& state) const = 0;
        virtual const std::string& Name() const = 0;
        virtual void Parse(boost::property_tree::ptree& pt, ParseState& state) = 0;
        virtual void Reset() = 0;
        virtual const ElementParserBase * const Parent() const {return m_parent;}
        virtual std::string Path() const
        {
            std::string result = "<" + Name() + ">";
            const ElementParserBase* el = m_parent;
            while (el!=NULL)
            {
                result = "<" + el->Name() + ">" + result;
                el = el->Parent();
            }
            return result;
        }
    private:
        const ElementParserBase* m_parent;
    };

    typedef boost::shared_ptr<ElementParserBase> ElementParserBasePtr;
    typedef std::vector<ElementParserBasePtr> ElementParserBaseVector;
    
    //------------------------------------------------------------
    //Helper element class to allow choice between two elements,
    //but can be expanded to allow any number of choices.
    //See the macros defined below the class.
    //------------------------------------------------------------
    template <class A, class B>
    class Choice : public ElementParserBase
    {
    public:
        explicit Choice() : m_parser() {}
        explicit Choice(const ElementParserBase* parent) : ElementParserBase(parent), m_parser() {}

        static bool MatchElementName(const std::string& name)
        {
            return A::MatchElementName(name) || B::MatchElementName(name);
        }

        static const std::string ElementName()
        {
            std::ostringstream os;
            os<<A::ElementName()<<" "<<B::ElementName();
            return os.str();
        }

        virtual bool Match(const std::string& name, ParseState& state) const 
        {
            if (m_parser)
            {
                if (m_parser->Match(name, state))
                {
                    return true;
                }
                else if (MatchElementName(name))
                {
                    std::stringstream ss;
                    ss<<"You cant have both '"<<A::ElementName()<<"' and '"<<B::ElementName()<<"' at location "<<Parent()->Path()<<". Choose one of them.";
                    throw ParseError("Element Missmatch", ss.str(), state.CurrentPath);
                }
                return false;
            }

            if (A::MatchElementName(name))
            {
                m_parser.reset(new A(Parent()));
                m_parser->Match(name, state); //necessary if m_parser is another Choice since the static version can't instatiate m_parser.
                return true;
            }
            else if (B::MatchElementName(name))
            {
                m_parser.reset(new B(Parent()));
                m_parser->Match(name, state); //necessary if m_parser is another Choice since the static version can't instatiate m_parser.
                return true;
            }
            return false;
        }

        virtual const std::string& Name() const 
        {
            assert(m_parser);
            return m_parser->Name(); //Will crash if called before a successfull call to Match. By design.
        }

        virtual void Parse(boost::property_tree::ptree& pt, ParseState& state) 
        {
            assert(m_parser);
            m_parser->Parse(pt, state); //Will crash if called before a successfull call to Match. By design.
        }

        virtual void Reset()
        {
            m_parser.reset(); //m_parser set to 0, and is now ready to match any of A and B again.
        }

    private:
        mutable ElementParserBasePtr m_parser; //This is instatiated during first call to Match.
    };
    //Macros for choice between more than two alternatives. Expand further if needed.
    #define ELEMENT_CHOICE_2(A,B) Choice<A, B>
    #define ELEMENT_CHOICE_3(A,B,C) Choice<A, ELEMENT_CHOICE_2(B, C) >
    #define ELEMENT_CHOICE_4(A,B,C,D) Choice<A, ELEMENT_CHOICE_3(B,C,D) >
    #define ELEMENT_CHOICE_5(A,B,C,D) Choice<A, ELEMENT_CHOICE_4(B,C,D,E) >

    //------------------------------------------------------------
    //Helper element class that allows an element to be ignored
    //------------------------------------------------------------
    template <int ElemName>
    class Ignore : public ElementParserBase
    {
    public: 
        explicit Ignore() {}
        explicit Ignore(const ElementParserBase* parent) : ElementParserBase(parent) {}
        static const std::string& ElementName(){return ElementNames::Instance().String(ElemName);}
        static bool MatchElementName(const std::string& name){return ElementName()==name;}
        virtual bool Match(const std::string& name, ParseState&) const {return MatchElementName(name);}
        virtual const std::string& Name() const {return ElementName();}
        virtual void Parse(boost::property_tree::ptree&, ParseState&) {}
        virtual void Reset() {}
    };

    //---------------------------------------------------------------------------------
    //Generic element class representing an element and contains all its subelements.
    //---------------------------------------------------------------------------------
    template <  int ElemName,
                class Occurrence,
                class SubElem = boost::mpl::vector<>,
                class Algorithm = ParseAlgorithm<ElemName> >
    class Element : public ElementParserBase
    {
    public:

        explicit Element() : ElementParserBase(), m_subElements(), m_occurrences(), m_parseAlgorithm()
        {
            InitSubElements< boost::mpl::size<SubElem>::type::value - 1 >();
        }

        explicit Element(const ElementParserBase* parent) : ElementParserBase(parent), m_subElements(), m_occurrences()
        {
            InitSubElements< boost::mpl::size<SubElem>::type::value - 1 >();
        }

        static const std::string& ElementName()
        {
            return ElementNames::Instance().String(ElemName);
        }

        static bool MatchElementName(const std::string& name)
        {
            return ElementName()==name;
        }

        virtual const std::string& Name() const
        {
            return ElementName();
        }
        
        virtual bool Match(const std::string& name, ParseState& /*state*/) const 
        {
            return MatchElementName(name);
        }

        virtual void Reset()
        {
            m_occurrences.Reset();
            std::for_each(m_subElements.begin(), m_subElements.end(), boost::bind(&ElementParserBase::Reset, _1));
        }

        virtual void Parse(boost::property_tree::ptree& pt, ParseState& state)
        {
            CheckOccurrence(state);

            //Handle element data.
            m_parseAlgorithm(pt, state);

            //Handle sub elements
            for (boost::property_tree::ptree::iterator it = pt.begin(); it!=pt.end(); ++it)
            {             
                static std::string xmlComment = ElementNames::Instance().String(ElementNames::XmlComment);
                if (it->first==xmlComment) //Instead of adding Ignore<XmlComment> to every element we hard-code it here.
                {
                    continue;
                }

                //it->first = ElementName, it->second = subTree
                ElementParserBaseVector::iterator match = std::find_if( m_subElements.begin(), 
                                                                        m_subElements.end(), 
                                                                        boost::bind(&ElementParserBase::Match, _1, boost::cref(it->first), boost::ref(state)));
                if (match!=m_subElements.end())
                {
                    (*match)->Parse(it->second, state);
                }
                else
                {
                    std::ostringstream ss;
                    ss<<"Element '"<<it->first<<"' is not expected at location "<<Path();
                    throw ParseError("Unexpected Element", ss.str(), state.CurrentPath);
                }
            }
        }
       
    private:
        ElementParserBaseVector m_subElements;
        Occurrence m_occurrences;
        Algorithm m_parseAlgorithm;

        void CheckOccurrence(ParseState& state)
        {
            ++m_occurrences;
            if (!m_occurrences)
            {
                std::ostringstream ss;
                ss<<"Element '"<<Name()<<"' only allowed to occur between "<<m_occurrences.MinOccurrences<<" and "<<m_occurrences.MaxOccurrences<<" times at location "<<Parent()->Path();
                throw ParseError("Wrong number of occurrences", ss.str(), state.CurrentPath);
            }
            std::for_each(m_subElements.begin(), m_subElements.end(), boost::bind(&ElementParserBase::Reset, _1));
        }

        template <int N> void InitSubElements()
        {            
            //Here we go through all the types in SubElem and instantiates them with 'this' as parent constructor arg.
            ElementParserBasePtr subEl(new boost::mpl::at< SubElem, boost::mpl::int_<N> >::type(this));
            m_subElements.push_back(subEl);
            InitSubElements<N-1>();
        }
        template <> void InitSubElements<-1>(){/*m_subElements is now completly instantiated*/}
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
