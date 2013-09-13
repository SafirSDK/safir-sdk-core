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
#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/timer.hpp>
#include "ParseJob.h"
#include "ElementParserDefs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    ParseJob::ParseJob(const boost::filesystem::path &definitions, size_t maxNumberOfThreads)
        :m_result()
    {
        size_t numFiles=NumberOfFiles(definitions);
        size_t numWorkers=CalcNumberOfWorkers(maxNumberOfThreads, numFiles);
        std::cout<<"        Starting ParseJob #files: "<<numFiles<<", #threads: "<<numWorkers<<std::endl;

        //Create worker threads that will parse all dou-files. Dom-files are also
        //read into propertyTrees and stored in domFiles for later parsing.
        Futures futures;
        XmlVec domFiles;
        CreateWorkers(definitions, numWorkers, numFiles, futures, domFiles);

        //Get all the parseStates form futures and handle if exception occurred during parsing.
        std::vector<ParseStatePtr> states;
        CollectParseStates(futures, states);

        //After dou files are parsed, we must do some completion stuff before the repository is ready to use.
        RepositoryCompletionAlgorithms postProcessing(m_result);
        postProcessing.DouParsingCompletion(states);

        //did we got this far, the repository is complete but still without any propertyMappings

        //Parse dom-files
        ParseJob::Worker<DomParser> domWorker(m_result);
        domWorker.m_xml->swap(domFiles);
        boost::shared_ptr<Task> task(new Task(domWorker));
        //futures.push_back(Future(task->get_future()));
        Future fut(task->get_future());
        boost::thread thd(boost::bind(&Task::operator(), task));
        thd.detach();
        ParseStatePtr domState=fut.get();
        std::vector<ParseStatePtr> domStates;
        domStates.push_back(domState);
        postProcessing.DomParsingCompletion(domStates);
    }    

    void ParseJob::CreateWorkers(const boost::filesystem::path& definitions,
                                 size_t numberOfWorkers,
                                 size_t numberOfFiles,
                                 Futures& futures,
                                 XmlVec& domFiles)
    {
        enum FileType{UnhandledFileType, DouFileType, DomFileType};

        const size_t filesPerWorker=numberOfFiles/numberOfWorkers;
        boost::filesystem::recursive_directory_iterator it(definitions), end;
        futures.reserve(numberOfWorkers);
        for (size_t workerCount=0; workerCount<numberOfWorkers; ++workerCount)
        {
            ParseJob::Worker<DouParser> worker;
            size_t fileCount=0;

            //Add files to current worker            
            while (it!=end)
            {
                FileType fileType=UnhandledFileType;
                if (it->path().extension() == ".dou")
                {
                    fileType=DouFileType;
                }
                else if (it->path().extension()==".dom")
                {
                    fileType=DomFileType;
                }

                if (fileType!=UnhandledFileType)
                {
                    boost::shared_ptr<boost::property_tree::ptree> pt(new boost::property_tree::ptree);
                    try
                    {
                        boost::property_tree::read_xml(it->path().string(), *pt, boost::property_tree::xml_parser::trim_whitespace);
                    }
                    catch (boost::property_tree::xml_parser_error& err) //cant catch as const-ref due to bug in early boost versions.
                    {
                        std::ostringstream ss;
                        ss<<err.message()<<". Line: "<<err.line();
                        throw ParseError("Invalid XML", ss.str(), it->path().string(), 10);;
                    }

                    switch (fileType)
                    {
                    case DouFileType:
                        worker.m_xml->push_back(std::make_pair(it->path().string(), pt));
                        break;
                    case DomFileType:
                        domFiles.push_back(std::make_pair(it->path().string(), pt)); //DomFiles are saved to later
                        break;
                    case UnhandledFileType:
                        break;
                    }
                }

                ++it; //increment file iterator

                if (++fileCount>=filesPerWorker && workerCount<numberOfWorkers-1)
                {
                    //Enough files for this worker and it is not the last worker. Last worker has to take the rest of files.
                    break;
                }
            }

            //Start the worker
            boost::shared_ptr<Task> task(new Task(worker));
            futures.push_back(Future(task->get_future()));
            boost::thread thd(boost::bind(&Task::operator(), task));
            thd.detach();
        }
    }

    size_t ParseJob::NumberOfFiles(const boost::filesystem::path& definitions) const
    {
        boost::filesystem::recursive_directory_iterator it(definitions), end;
        return std::distance(it, end);
    }

    size_t ParseJob::CalcNumberOfWorkers(size_t maxThreads, size_t files) const
    {
        if (files<100)
        {
            return 1;
        }
        return std::min(files/100, maxThreads);
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
