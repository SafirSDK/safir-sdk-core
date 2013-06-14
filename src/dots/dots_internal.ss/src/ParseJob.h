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
#ifndef __DOTS_INTERNAL_PARSE_JOB_H__
#define __DOTS_INTERNAL_PARSE_JOB_H__

#ifdef _MSC_VER
#pragma warning(disable:4702) //unreachable code
#endif
#include <boost/thread/future.hpp>
#ifdef _MSC_VER
#pragma warning(default:4702)
#endif

#include <boost/thread.hpp>
#include <boost/filesystem.hpp>
#include <boost/property_tree/ptree.hpp>
#include "ParseState.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    //-----------------------------------------------------------------------------------------
    //Parses a location with dou/dom-files. Will start worker-threads depending on the number
    //of files to parse. Will never start more threads than specified by maxNumberOfThreads.
    //Constructor will do all the work, throws ParseError on failure. Of success
    //result is collected by calling GetResult.
    //-----------------------------------------------------------------------------------------
    class ParseJob : public boost::noncopyable
    {
    public:        
        ParseJob(const boost::filesystem::path& definitions, size_t maxNumberOfThreads);
        boost::shared_ptr<TypeRepository> GetResult() {return m_result;}

    private:
        typedef std::pair< std::string, boost::shared_ptr<boost::property_tree::ptree> > XmlDef;
        typedef std::vector<XmlDef> XmlVec;
        typedef boost::packaged_task<ParseStatePtr> Task;
        typedef boost::shared_future<ParseStatePtr> Future;
        typedef std::vector< Future > Futures;
        boost::shared_ptr<RepositoryBasic> m_result;

        void CreateWorkers(const boost::filesystem::path& definitions,
                           size_t numberOfWorkers,
                           size_t numberOfFiles,
                           Futures& futures,
                           XmlVec& domFiles);
        size_t NumberOfFiles(const boost::filesystem::path& definitions) const;
        size_t CalcNumberOfWorkers(size_t maxThreads, size_t files) const;

        void CollectParseStates(Futures& futures, std::vector<ParseStatePtr>& states) const;

        template <class ParserT>
        struct Worker
        {
            Worker() : m_xml(new XmlVec), m_rep(new RepositoryBasic) {}
            Worker(const boost::shared_ptr<RepositoryBasic>& r) : m_xml(new XmlVec), m_rep(r) {}
            boost::shared_ptr<XmlVec> m_xml; //since Worker objects are copied into packaged task, we just want a pointer copy
            boost::shared_ptr<RepositoryBasic> m_rep;

            ParseStatePtr operator()()
            {
                ParseStatePtr state(new ParseState(m_rep));
                for (XmlVec::const_iterator it=m_xml->begin(); it!=m_xml->end(); ++it)
                {
                    try
                    {
                        state->currentPath=it->first;
                        state->propertyTree=it->second;
                        ParserT parser;
                        boost::property_tree::ptree::iterator ptIt=state->propertyTree->begin();
                        if (parser.Match(ptIt->first, *state))
                        {
                            parser.Parse(ptIt->second, *state);
                            parser.Reset(*state);
                        }
                    }
                    catch (const ParseError& err)
                    {
                        throw boost::enable_current_exception(err);
                    }
                    catch (const std::exception& err)
                    {
                        throw boost::enable_current_exception(ParseError("Unexpected Error", err.what(), state->currentPath, 11));
                    }
                    catch(...)
                    {
                        throw boost::enable_current_exception(ParseError("Programming Error", "You have found a bug in DotsParser. Please save the this dou-file and attach it to your bug report.", state->currentPath, 0));
                    }
                }

                return state;
            }
        };
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
