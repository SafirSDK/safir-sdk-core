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
                    if (thisRootFiles.find(filename)!=thisRootFiles.end())
                    {
                        std::ostringstream os;
                        os<<"The directory '"<<rootDirIt->string()<<"' constains duplicated version of file '"<<filename.string()<<"'"<<std::endl;
                        throw ParseError("Duplicated dou/dom file", os.str(), rootDirIt->string(), 2);
                    }
                    thisRootFiles.insert(filename);

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
    }

    ParseJob::ParseJob(const std::vector<boost::filesystem::path>& roots, size_t maxNumberOfThreads)
        :m_result()
    {
        std::map<boost::filesystem::path, boost::filesystem::path> douFiles;
        std::map<boost::filesystem::path, boost::filesystem::path> domFiles;
        SelectFiles(roots, douFiles, domFiles);
        size_t numWorkers=CalcNumberOfWorkers(maxNumberOfThreads, douFiles.size());
        lllout<<"        Starting ParseJob #files (dou/dom/tot): "<<douFiles.size()<<"/"<<domFiles.size()<<"/"<<douFiles.size()+domFiles.size()<<", #threads: "<<numWorkers<<std::endl;

        //Create worker threads that will parse all dou-files. Dom-files are also
        //read into propertyTrees and stored in domFiles for later parsing.
        Futures futures;        
        CreateDouWorkers(douFiles, numWorkers, futures);

        //Get all the parseStates form futures and handle if exception occurred during parsing.
        std::vector<ParseStatePtr> states;
        CollectParseStates(futures, states);

        //After dou files are parsed, we must do some completion stuff before the repository is ready to use.
        RepositoryCompletionAlgorithms postProcessing(m_result);
        postProcessing.DouParsingCompletion(states);

        //did we get this far, the repository is complete but still without any propertyMappings

        //Parse dom-files
        ParseJob::Worker<DomParser> domWorker(m_result, domFiles.begin(), domFiles.end());
        boost::shared_ptr<Task> domTask(new Task(domWorker));
        Future fut(domTask->get_future());
        boost::thread thd(boost::bind(&Task::operator(), domTask));
        thd.detach();
        ParseStatePtr domState=fut.get();
        std::vector<ParseStatePtr> domStates;
        domStates.push_back(domState);
        postProcessing.DomParsingCompletion(domStates);
    }

    void ParseJob::CreateDouWorkers(const std::map<boost::filesystem::path, boost::filesystem::path>& douFiles,
                                    size_t numberOfWorkers,
                                    Futures& futures)
    {
        const size_t filesPerWorker=douFiles.size()/numberOfWorkers;
        std::map<boost::filesystem::path, boost::filesystem::path>::const_iterator it=douFiles.begin();
        for (size_t i=0; i<numberOfWorkers-1; ++i)
        {
            std::map<boost::filesystem::path, boost::filesystem::path>::const_iterator start=it;
            for (size_t j=0; j<filesPerWorker; ++j)
            {
                ++it;
            }
            ParseJob::Worker<DouParser> worker(start, it);
            boost::shared_ptr<Task> task(new Task(worker));
            futures.push_back(Future(task->get_future()));
            boost::thread thd(boost::bind(&Task::operator(), task));
            thd.detach();
        }
        //last worker takes the rest
        ParseJob::Worker<DouParser> worker(it, douFiles.end());
        boost::shared_ptr<Task> task(new Task(worker));
        futures.push_back(Future(task->get_future()));
        boost::thread thd(boost::bind(&Task::operator(), task));
        thd.detach();
    }

    size_t ParseJob::CalcNumberOfWorkers(size_t maxThreads, size_t files) const
    {
        if (files<200)
        {
            return 1;
        }
        return std::min(files/200, maxThreads);
    }

    void ParseJob::CollectParseStates(Futures& futures, std::vector<ParseStatePtr>& states) const
    {        
        states.reserve(futures.size());
        for (Futures::iterator it=futures.begin(); it!=futures.end(); ++it)
        {
            try
            {
                states.push_back(it->get());
            }
            catch(...)
            {
                ParseError* parseErr=boost::current_exception_cast<ParseError>();
                if (parseErr)
                    throw *parseErr;
                else
                    throw; //This was an totally unexpected exception
            }
        }
    }
}
}
}
} //end namespace Safir::Dob::Typesystem
