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
#ifndef __DOTS_INTERNAL_PARSE_JOB_H__
#define __DOTS_INTERNAL_PARSE_JOB_H__

#include <boost/thread/mutex.hpp>

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
#include <Safir/Dob/Typesystem/Internal/ParseError.h>
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
        ParseJob(const std::vector<boost::filesystem::path>& roots, size_t maxNumberOfThreads);
        boost::shared_ptr<TypeRepository> GetResult() {return m_result;}

    private:
        typedef boost::packaged_task<ParseStatePtr> Task;
        typedef boost::shared_future<ParseStatePtr> Future;
        typedef std::vector< Future > Futures;
        boost::shared_ptr<RepositoryBasic> m_result;

        void CreateDouWorkers(const std::map<boost::filesystem::path, boost::filesystem::path>& douFiles,
                              size_t numberOfWorkers,
                              Futures& futures);

        size_t CalcNumberOfWorkers(size_t maxThreads, size_t files) const;

        void CollectParseStates(Futures& futures, std::vector<ParseStatePtr>& states) const;
    };
}
}
}
} //end namespace Safir::Dob::Typesystem

#endif
