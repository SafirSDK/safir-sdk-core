/******************************************************************************
*
* Copyright Saab AB, 2004-2015 (http://safirsdkcore.com)
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
#ifndef __DOTS_INTERNAL_ELEMENT_PARSER_BASE_H__
#define __DOTS_INTERNAL_ELEMENT_PARSER_BASE_H__

#include <iostream>
#include <string>
#include <vector>
#include <stack>
#include <assert.h>
#include <algorithm>
#include <boost/mpl/at.hpp>
#include <boost/mpl/vector.hpp>
#include "ParseAlgorithms.h"
#include "OccurrenceRules.h"

//------------------------------------------------------------------------------------
// Implementation of the xml-parser engine.
// Will iterate over all elements in the files and call appropriate ParseAlgortihm
//------------------------------------------------------------------------------------
namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    //------------------------------------------------------------
    //Abstract base class for all parsers.
    //------------------------------------------------------------
    class ElementParserBase
    {
    public:
        explicit ElementParserBase() : m_parent(NULL) {}
        explicit ElementParserBase(const ElementParserBase* parent) : m_parent(parent) {}
        virtual ~ElementParserBase() = default;
        virtual bool Match(const std::string& name, ParseState& state) const=0;
        virtual const std::string& Name() const=0;
        virtual void Parse(boost::property_tree::ptree& pt, ParseState& state)=0;
        virtual void Reset(ParseState& state)=0;
        virtual const ElementParserBase * Parent() const {return m_parent;}
        virtual std::string Path() const
        {
            std::string result="<" + Name() + ">";
            const ElementParserBase* el=m_parent;
            while (el!=NULL)
            {
                result="<";
                result += el->Name();
                result += ">";
                result += result;
                el=el->Parent();
            }
            return result;
        }
    private:
        const ElementParserBase* m_parent;
    };

    typedef std::shared_ptr<ElementParserBase> ElementParserBasePtr;
    typedef std::vector<ElementParserBasePtr> ElementParserBaseVector;

    //----------------------------------------------------------------------------
    // Instantiator class. Instantiates a complete ElementTree based on typelists.
    // See definitions in ElementParserDefs.
    //----------------------------------------------------------------------------
    template < class ElementTypeVector, int Index >
    struct ElementInstantiator
    {
        typedef typename boost::mpl::at< ElementTypeVector, boost::mpl::int_<Index> >::type type;
        void operator()(ElementParserBase* current, ElementParserBaseVector& subElements)
        {
            ElementParserBasePtr subEl(std::make_shared<type>(current));
            subElements.push_back(subEl);
            ElementInstantiator< ElementTypeVector, Index-1 >()(current, subElements);
        }
    };

    template < class ElementTypeVector >
    struct ElementInstantiator<ElementTypeVector, -1> //This is the end condition in the element instatiation algorithm.
    {
        void operator()(ElementParserBase*, ElementParserBaseVector&){}
    };

    //------------------------------------------------------------
    //Helper element class to allow choice between two elements,
    //but can be expanded to allow any number of choices.
    //See the macros defined below the class.
    //------------------------------------------------------------
    template <class A, class B, class Occurrence>
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

        bool Match(const std::string& name, ParseState& state) const override
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
                    throw ParseError("Element Mismatch", ss.str(), state.currentPath, 12);
                }
                return false;
            }

            if (A::MatchElementName(name))
            {
                m_parser=std::make_shared<A>(Parent());
                m_parser->Match(name, state); //necessary if m_parser is another Choice since the static version can't instatiate m_parser.
                return true;
            }
            else if (B::MatchElementName(name))
            {
                m_parser=std::make_shared<B>(Parent());
                m_parser->Match(name, state); //necessary if m_parser is another Choice since the static version can't instatiate m_parser.
                return true;
            }
            return false;
        }

        const std::string& Name() const override
        {
            assert(m_parser);
            return m_parser->Name(); //Will crash if called before a successfull call to Match. By design.
        }

        void Parse(boost::property_tree::ptree& pt, ParseState& state) override
        {
            assert(m_parser);
            m_parser->Parse(pt, state); //Will crash if called before a successfull call to Match. By design.
        }

        void Reset(ParseState& state) override
        {
            if (m_parser)
            {
                m_parser->Reset(state);
            }
            else if (!m_occurrences)
            {
                std::ostringstream ss;
                ss<<"Expecting one of following elements: "<<ElementName();

                // Check if this error happened in a Paramter. In that case try to give info about wich parameter is the problem.
                if (Parent() && Parent()->Name() == Elements::Parameter::Name() && state.lastInsertedClass->ownParameters.back()->GetName())
                {
                    ss << ". Error in parameter '" << state.lastInsertedClass->ownParameters.back()->GetName() << "'.";
                }
                throw ParseError("Wrong number of occurrences", ss.str(), state.currentPath, 13);
            }
            m_parser.reset(); //m_parser set to 0, and is now ready to match any of A and B again.
            m_occurrences.Reset();
        }

    private:
        Occurrence m_occurrences;
        mutable ElementParserBasePtr m_parser; //This is instatiated during first call to Match.
    };
    //Macros for choice between more than two alternatives. Expand further if needed.
    #define ELEMENT_CHOICE_2(A,B,Occurrence) Choice<A, B, Occurrence >
    #define ELEMENT_CHOICE_3(A,B,C,Occurrence) Choice<A, ELEMENT_CHOICE_2(B, C, Occurrence), Occurrence >
    #define ELEMENT_CHOICE_4(A,B,C,D,Occurrence) Choice<A, ELEMENT_CHOICE_3(B,C,D,Occurrence), Occurrence >
    #define ELEMENT_CHOICE_5(A,B,C,D,E,Occurrence) Choice<A, ELEMENT_CHOICE_4(B,C,D,E,Occurrence), Occurrence >
    #define ELEMENT_CHOICE_6(A,B,C,D,E,F,Occurrence) Choice<A, ELEMENT_CHOICE_5(B,C,D,E,F,Occurrence), Occurrence >
    #define ELEMENT_CHOICE_7(A,B,C,D,E,F,G,Occurrence) Choice<A, ELEMENT_CHOICE_6(B,C,D,E,F,G,Occurrence), Occurrence >
    #define ELEMENT_CHOICE_8(A,B,C,D,E,F,G,H,Occurrence) Choice<A, ELEMENT_CHOICE_7(B,C,D,E,F,G,H,Occurrence), Occurrence >
    #define ELEMENT_CHOICE_9(A,B,C,D,E,F,G,H,I,Occurrence) Choice<A, ELEMENT_CHOICE_8(B,C,D,E,F,G,H,I,Occurrence), Occurrence >
    #define ELEMENT_CHOICE_10(A,B,C,D,E,F,G,H,I,J,Occurrence) Choice<A, ELEMENT_CHOICE_9(B,C,D,E,F,G,H,I,J,Occurrence), Occurrence >

    //------------------------------------------------------------
    //Helper element class that allows an element to be ignored
    //------------------------------------------------------------
    template <class ElementT>
    class Ignore : public ElementParserBase
    {
    public:
        explicit Ignore() {}
        explicit Ignore(const ElementParserBase* parent) : ElementParserBase(parent) {}
        static const std::string& ElementName(){return ElementT::Name();}
        static bool MatchElementName(const std::string& name){return ElementName()==name;}
        bool Match(const std::string& name, ParseState&) const override {return MatchElementName(name);}
        const std::string& Name() const override {return ElementName();}
        void Parse(boost::property_tree::ptree&, ParseState&) override  {}
        void Reset(ParseState&) override  {}
    };

    class IgnoreAny : public ElementParserBase
    {
    public:
        explicit IgnoreAny() {}
        explicit IgnoreAny(const ElementParserBase* parent) : ElementParserBase(parent) {}
        bool Match(const std::string&, ParseState&) const override {return true;}
        const std::string& Name() const override {static std::string dummy=""; return dummy;}
        void Parse(boost::property_tree::ptree&, ParseState&) override  {}
        void Reset(ParseState&) override  {}
    };

    //-------------------------------
    // Element matching algorithms
    //-------------------------------
    //this is the normal matcher alg, compares elements
    struct ElementNameMatcher {static bool Match(const std::string& element , const std::string& name) {return element==name;}};
    //this alg matches anything, if used be sure its added as the last element to be checked since it will swallow all elements.
    struct AnyMatcher{static bool Match(const std::string& /*element*/, const std::string& /*name*/) {return true;}};

    //---------------------------------------------------------------------------------
    //Generic element class representing an element and contains all its subelements.
    //---------------------------------------------------------------------------------
    template <  class ElementT,
                class Occurrence,
                class SubElem=boost::mpl::vector<>,
                class Algorithm=ParseAlgorithm<ElementT>,
                class MatchAlg=ElementNameMatcher >
    class Element : public ElementParserBase
    {
    public:

        Element()
            :ElementParserBase()
            ,m_used(false)
            ,m_subElements()
            ,m_occurrences()
            ,m_parseAlgorithm()
        {
            ElementInstantiator< SubElem, boost::mpl::size<SubElem>::type::value - 1 >()(this, m_subElements);
        }

        explicit Element(const ElementParserBase* parent)
            :ElementParserBase(parent)
            ,m_used(false)
            ,m_subElements()
            ,m_occurrences()
        {
            ElementInstantiator< SubElem, boost::mpl::size<SubElem>::type::value - 1 >()(this, m_subElements);
        }

        static const std::string& ElementName()
        {
            return ElementT::Name();
        }

        static bool MatchElementName(const std::string& name)
        {
            return MatchAlg::Match(ElementName(), name);
        }

        const std::string& Name() const override
        {
            return ElementName();
        }

        bool Match(const std::string& name, ParseState& /*state*/) const override
        {
            return MatchElementName(name);
        }

        void Reset(ParseState& state) override
        {
            if (m_used)
            {
                std::for_each(m_subElements.begin(),
                              m_subElements.end(),
                              [&state](const ElementParserBasePtr& el){el->Reset(state);});
            }
            CheckOccurrence(state);
            m_used=false;
            m_occurrences.Reset();
        }

//VS2015 will warn that everything below "m_parseAlgorithm(pt, state);" below is unreachable
//for some unstantiation.
#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4702)
#endif
        void Parse(boost::property_tree::ptree& pt, ParseState& state) override
        {
            if (m_used)
            {
                std::for_each(m_subElements.begin(),
                              m_subElements.end(),
                              [&state](const ElementParserBasePtr& el){el->Reset(state);});
            }
            m_used=true;
            ++m_occurrences;
            CheckOccurrence(state);

            //Handle element data.
            m_parseAlgorithm(pt, state);

            //Handle sub elements
            for (boost::property_tree::ptree::iterator it=pt.begin(); it!=pt.end(); ++it)
            {
                //it->first=ElementName, it->second=subTree
                ElementParserBaseVector::iterator match =
                    std::find_if(m_subElements.begin(),
                                 m_subElements.end(),
                                 [&state,it](const ElementParserBasePtr& el){return el->Match(it->first,state);});

                if (match!=m_subElements.end())
                {
                    (*match)->Parse(it->second, state);
                }
                else
                {
                    std::ostringstream ss;
                    ss<<"Element '"<<it->first<<"' is not expected at location "<<Path();
                    throw ParseError("Unexpected Element", ss.str(), state.currentPath, 14);
                }
            }
        }

#ifdef _MSC_VER
#pragma warning (pop)
#endif

    private:
        bool m_used;
        ElementParserBaseVector m_subElements;
        Occurrence m_occurrences;
        Algorithm m_parseAlgorithm;

        void CheckOccurrence(ParseState& state)
        {
            if (!m_occurrences)
            {
                std::ostringstream ss;
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable:4127) //Get rid of warning that this if-expression is constant (comparing two constants)
#endif
                if (m_occurrences.MinOccurrences == m_occurrences.MaxOccurrences)
                {
                    ss<<"Element '"<<Name()<<"' must occur exactly "<<m_occurrences.MinOccurrences<<" time(s) at location "<<Parent()->Path()<<". Number of occurrences="<<m_occurrences();
                }
                else if (m_occurrences.MaxOccurrences == INT_MAX)
                {
                    ss<<"Element '"<<Name()<<"' must occur at least "<<m_occurrences.MinOccurrences<<" times at location "<<Parent()->Path()<<". Number of occurrences="<<m_occurrences();
                }
                else
                {
                    ss<<"Element '"<<Name()<<"' only allowed to occur between "<<m_occurrences.MinOccurrences<<" and "<<m_occurrences.MaxOccurrences<<" times at location "<<Parent()->Path()<<". Number of occurrences="<<m_occurrences();
                }
#ifdef _MSC_VER
#pragma warning(pop)
#endif
                throw ParseError("Wrong number of occurrences", ss.str(), state.currentPath, 15);
            }
        }
    };
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport

#endif
