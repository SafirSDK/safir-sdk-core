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
#include <functional>
#include <iostream>
#include <fstream>
#include <future>
#include <thread>
#include <chrono>
#include <algorithm>
#include <boost/make_shared.hpp>
#include <boost/thread.hpp>
#include <Safir/Utilities/Internal/LowLevelLogger.h>
#include "ParseJob.h"
#include "SchemaDefinitions.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
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
                            lllog(9)<<"The file '"<<inserted.first->second.string().c_str()<<"' will be overridden by file '"<<fp.string().c_str()<<"'"<<std::endl;
                            inserted.first->second=fp; //update to overriding path
                        }
                    }
                    ++fileIt;
                }
            }
        }

        // Open all files in a lot of threads might speed up the parsing on Windows. Seems to have almost no effect on Linux.
        inline std::vector<std::pair<std::shared_ptr<boost::property_tree::ptree>, std::string>> ReadFiles(const std::map<boost::filesystem::path, boost::filesystem::path>& files)
        {
            std::vector<std::string> filePaths;
            for (auto it =  std::cbegin(files); it != std::cend(files); ++it)
            {
                filePaths.push_back(it->second.string());
            }

            size_t numThreads = files.size() < 100 ? 1 : (files.size() < 1000 ? 25 : 50); // we can do something better here
            size_t filesPerThread = files.size() / numThreads;

            std::vector<std::pair<std::future<std::vector<std::pair<std::shared_ptr<boost::property_tree::ptree>, std::string>>>, std::thread>> threads;
            for (size_t thNum = 0; thNum < numThreads; ++thNum)
            {
                size_t startIx = thNum * filesPerThread;
                size_t endIx = (thNum < numThreads - 1) ?  (startIx + filesPerThread) : filePaths.size();

                std::packaged_task<std::vector<std::pair<std::shared_ptr<boost::property_tree::ptree>, std::string>>()> pt([startIx, endIx, &filePaths]{

                    std::vector<std::pair<std::shared_ptr<boost::property_tree::ptree>, std::string>> taskResult;
                    for (auto fileIx = startIx; fileIx < endIx; ++fileIx)
                    {
                        std::ifstream is;
                        is.open(filePaths[fileIx]);
                        if (is.is_open())
                        {
                            is.close();
                        }

                        std::shared_ptr<boost::property_tree::ptree> propTree=std::make_shared<boost::property_tree::ptree>();
                        try
                        {
                            boost::property_tree::read_xml(filePaths[fileIx], *propTree, boost::property_tree::xml_parser::no_comments);
                            taskResult.push_back(std::make_pair(propTree, filePaths[fileIx]));
                        }
                        catch (const boost::property_tree::xml_parser_error&)
                        {
                            propTree = nullptr;
                            taskResult.push_back(std::make_pair(propTree, filePaths[fileIx]));
                            break;
                        }
                    }
                    return taskResult;
                });

                threads.emplace_back(pt.get_future(), std::move(pt));
            }

            // wait for all threads
            std::vector<std::pair<std::shared_ptr<boost::property_tree::ptree>, std::string>> result;
            result.reserve(files.size());
            for (auto& t : threads)
            {
                t.second.join();
                auto taskResult = t.first.get();
                result.insert(result.begin(), taskResult.begin(), taskResult.end());
            }

            return result;
        }

        template <class ParserT, class CompletionAlg>
        struct ParseWorker
        {
            void operator()(const std::shared_ptr<RepositoryLocal>& repository, const std::vector<std::pair<std::shared_ptr<boost::property_tree::ptree>, std::string>>& sources)
            {
                ParseState state(repository);

                for (auto& src : sources)
                {
                    if (src.first == nullptr)
                    {
                        try
                        {
                            boost::property_tree::read_xml(src.second, *(src.first), boost::property_tree::xml_parser::no_comments);
                        }
                        catch (const boost::property_tree::xml_parser_error& err)
                        {
                            std::ostringstream ss;
                            ss<<err.message()<<". Line: "<<err.line();
                            throw ParseError("Invalid XML", ss.str(), src.second, 10);
                        }
                    }

                    try
                    {
                        state.currentPath=src.second;
                        state.propertyTree=src.first;
                        ParserT parser;
                        boost::property_tree::ptree::iterator ptIt=state.propertyTree->begin();
                        if (parser.Match(ptIt->first, state))
                        {
                            parser.Parse(ptIt->second, state);
                            parser.Reset(state);
                        }
                    }
                    catch (const ParseError& err)
                    {
                        throw ParseError(err.Label(), err.Description(), err.File(), err.ErrorId());
                    }
                    catch (const std::exception& err)
                    {
                        throw ParseError("Unexpected Error", err.what(), state.currentPath, 11);
                    }
                    catch(...)
                    {
                        throw ParseError("Programming Error", "You have found a bug in dots_internal. Please save the this dou-file and attach it to your bug report.", state.currentPath, 0);
                    }
                }

                CompletionAlg()(state);
            }
        };
    }

    ParseJob::ParseJob(const std::vector<boost::filesystem::path>& roots)
        :m_result(std::make_shared<RepositoryLocal>())
    {
        auto startTime = std::chrono::high_resolution_clock::now();

        std::map<boost::filesystem::path, boost::filesystem::path> douFiles;
        std::map<boost::filesystem::path, boost::filesystem::path> domFiles;
        SelectFiles(roots, douFiles, domFiles);

        lllog(5) << "Starting ParseJob #files (dou/dom/tot): "
                 << douFiles.size() << "/"
                 << domFiles.size() << "/"
                 << douFiles.size()+domFiles.size() << std::endl;

        // parse dou-files
        auto dou = ReadFiles(douFiles);
        ParseWorker<DouParser, DouCompletionAlgorithm>()(m_result, dou);

        // parse dom-files
        auto dom = ReadFiles(domFiles);
        ParseWorker<DomParser, DomCompletionAlgorithm>()(m_result, dom);

        auto endTime = std::chrono::high_resolution_clock::now();
        lllog(5)<<"ParseJob finished. Time used for dou-parsing: " << std::chrono::duration_cast<std::chrono::milliseconds>(endTime - startTime).count() / 1000.0 << " sec." <<std::endl;
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport
