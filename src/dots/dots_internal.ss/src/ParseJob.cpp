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
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/timer.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/asio.hpp>
#include <boost/make_shared.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "ParseJob.h"
#include "SchemaDefinitions.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    namespace
    {
        /**
         * Selects files to parse and handles file override.
         */
        inline void SelectFiles(const std::vector<boost::filesystem::path>& roots,
                                std::map<boost::filesystem::path, boost::filesystem::path>& douFiles,
                                std::map<boost::filesystem::path, boost::filesystem::path>& domFiles)
        {
            std::set<boost::filesystem::path> thisRootFiles;
            for (std::vector<boost::filesystem::path>::const_iterator rootDirIt=roots.begin(); rootDirIt!=roots.end(); ++rootDirIt)
            {
                thisRootFiles.clear();
                boost::filesystem::recursive_directory_iterator fileIt(*rootDirIt), fileItend;
                while (fileIt!=fileItend)
                {
                    const boost::filesystem::path& fp=fileIt->path();
                    const boost::filesystem::path filename=fp.filename();

                    std::map<boost::filesystem::path, boost::filesystem::path>* pmap=NULL;
                    if (fp.extension()==".dou")
                    {
                        pmap=&douFiles;
                    }
                    else if (fp.extension()==".dom")
                    {
                        pmap=&domFiles;
                    }

                    if (pmap)
                    {
                        if (thisRootFiles.find(filename)!=thisRootFiles.end())
                        {
                            std::ostringstream os;
                            os<<"The directory '"<<rootDirIt->string()<<"' constains duplicated version of file '"<<filename.string()<<"'"<<std::endl;
                            throw ParseError("Duplicated dou/dom file", os.str(), rootDirIt->string(), 2);
                        }
                        thisRootFiles.insert(filename);

                        std::pair<std::map<boost::filesystem::path, boost::filesystem::path>::iterator, bool> inserted=pmap->insert(std::make_pair(filename, fp));
                        if (!inserted.second)
                        {
                            //log and update
                            lllout<<"The file '"<<inserted.first->second.string().c_str()<<"' will be overridden by file '"<<fp.string().c_str()<<"'"<<std::endl;
                            inserted.first->second=fp; //update to overriding path
                        }
                    }
                    ++fileIt;
                }
            }
        }

        /**
         * Worker class run as separate future.
         */
        template <class ParserT>
        class ParseWorker
        {
        public:
            ParseWorker()
                :m_rep(new RepositoryBasic)
                ,m_state(new ParseState(m_rep))
                ,m_error()
            {
            }

            ParseWorker(const boost::shared_ptr<RepositoryBasic>& rep)
                :m_rep(rep)
                ,m_state(new ParseState(m_rep))
                ,m_error()
            {
            }

            void AddPath(const boost::filesystem::path& p)
            {
                m_paths.push_back(p);
            }

            ParseStatePtr GetResult() //throws if error occured in Run
            {
                if (!m_error)
                {
                    return m_state;
                }
                else
                {
                    throw *m_error; //throw ParseError
                }
            }

            void Run()
            {
                for (std::vector<boost::filesystem::path>::const_iterator pathIt=m_paths.begin(); pathIt!=m_paths.end(); ++pathIt)
                {
                    boost::shared_ptr<boost::property_tree::ptree> pt(new boost::property_tree::ptree);
                    try
                    {
                        boost::property_tree::read_xml(pathIt->string(), *pt, boost::property_tree::xml_parser::trim_whitespace | boost::property_tree::xml_parser::no_comments);
                    }
                    catch (boost::property_tree::xml_parser_error& err) //cant catch as const-ref due to bug in early boost versions.
                    {
                        std::ostringstream ss;
                        ss<<err.message()<<". Line: "<<err.line();
                        m_error.reset(new ParseError("Invalid XML", ss.str(), pathIt->string(), 10));
                        return;
                    }

                    try
                    {
                        m_state->currentPath=pathIt->string();
                        m_state->propertyTree=pt;
                        ParserT parser;
                        boost::property_tree::ptree::iterator ptIt=m_state->propertyTree->begin();
                        if (parser.Match(ptIt->first, *m_state))
                        {
                            parser.Parse(ptIt->second, *m_state);
                        }
                    }
                    catch (const ParseError& err)
                    {
                        m_error.reset(new ParseError(err.Label(), err.Description(), err.File(), err.ErrorId()));
                        return;
                    }
                    catch (const std::exception& err)
                    {
                        m_error.reset(new ParseError("Unexpected Error", err.what(), m_state->currentPath, 11));
                        return;
                    }
                    catch(...)
                    {
                        m_error.reset(new ParseError("Programming Error", "You have found a bug in dots_internal. Please save the this dou-file and attach it to your bug report.", m_state->currentPath, 0));
                        return;
                    }
                }
            }

        private:
            boost::shared_ptr<RepositoryBasic> m_rep;
            std::vector<boost::filesystem::path> m_paths;
            ParseStatePtr m_state;
            boost::shared_ptr<const ParseError> m_error;
        };
    }

    ParseJob::ParseJob(const std::vector<boost::filesystem::path>& roots, size_t maxNumberOfThreads)
        :m_result()
    {
        std::map<boost::filesystem::path, boost::filesystem::path> douFiles;
        std::map<boost::filesystem::path, boost::filesystem::path> domFiles;
        SelectFiles(roots, douFiles, domFiles);
        size_t numWorkers=std::max(size_t(1), std::min(douFiles.size()/500, maxNumberOfThreads)); //500 files per thread, but always in range 1..maxNumberOfThreads

        lllout<<"        Starting ParseJob #files (dou/dom/tot): "<<douFiles.size()<<"/"<<domFiles.size()<<"/"<<douFiles.size()+domFiles.size()<<", #threads: "<<numWorkers<<std::endl;

        //Create worker threads that will parse all dou-files.
        std::vector<ParseStatePtr> states;
        CreateAndRunWorkers(douFiles, numWorkers, states);

        //After dou files are parsed, we must do some completion stuff before the repository is ready to use.
        RepositoryCompletionAlgorithms postProcessing(m_result);
        postProcessing.DouParsingCompletion(states);

        //did we get this far, the repository is complete but still without any propertyMappings

        //Parse dom-files
        ParseWorker<DomParser> domWorker(m_result);
        for (std::map<boost::filesystem::path, boost::filesystem::path>::const_iterator it=domFiles.begin(); it!=domFiles.end(); ++it)
        {
            domWorker.AddPath(it->second);
        }
        domWorker.Run();
        std::vector<ParseStatePtr> domStates;
        domStates.push_back(domWorker.GetResult());
        postProcessing.DomParsingCompletion(domStates);
    }

    void ParseJob::CreateAndRunWorkers(const std::map<boost::filesystem::path, boost::filesystem::path>& douFiles,
                                       size_t numberOfWorkers,
                                       std::vector<ParseStatePtr>& states) const
    {
        if (numberOfWorkers==1)
        {
            //an optimization: do everything in this thread
            ParseWorker<DouParser> worker;
            for (std::map<boost::filesystem::path, boost::filesystem::path>::const_iterator it=douFiles.begin(); it!=douFiles.end(); ++it)
            {
                worker.AddPath(it->second);
            }
            worker.Run();
            states.push_back(worker.GetResult());
            return;
        }

        typedef boost::shared_ptr< ParseWorker<DouParser> > WorkPtr;

        const size_t filesPerWorker=douFiles.size()/numberOfWorkers;

        boost::asio::io_service ioService;
        std::vector<WorkPtr> workers;
        boost::thread_group threads;

        boost::scoped_ptr<boost::asio::io_service::work> keepAlive(new boost::asio::io_service::work(ioService));
        for (size_t i=0; i<numberOfWorkers; ++i)
        {
            threads.create_thread(boost::bind(&boost::asio::io_service::run, &ioService));
        }


        std::map<boost::filesystem::path, boost::filesystem::path>::const_iterator it=douFiles.begin();
        for (size_t workerCount=0; workerCount<numberOfWorkers; ++workerCount)
        {
            WorkPtr worker=boost::make_shared< ParseWorker<DouParser> >();
            for (size_t j=0; j<filesPerWorker; ++j)
            {
                worker->AddPath(it->second);
                ++it;
            }

            if (workerCount+1==numberOfWorkers)
            {
                //this is the last worker, add the remaining files
                for (; it!=douFiles.end(); ++it)
                {
                    worker->AddPath(it->second);
                }
            }
            workers.push_back(worker);
            ioService.post(boost::bind(&ParseWorker<DouParser>::Run, worker));
        }
        keepAlive.reset(); //remove dummy work that keeps ioService alive when no real work exists
        threads.join_all();
        ioService.stop();

        //collect all results
        for (std::vector<WorkPtr>::iterator workerIt=workers.begin(); workerIt!=workers.end(); ++workerIt)
        {
            states.push_back((*workerIt)->GetResult()); //GetResult will throw ParseError if an error occured during parsing
        }
    }
}
}
}
} //end namespace Safir::Dob::Typesystem
