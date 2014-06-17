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
* GNU General Public License for more Internals.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#include <boost/bind.hpp>
#include <boost/function.hpp>
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
                            lllout<<"The file '"<<inserted.first->second.string().c_str()<<"' will be overridden by file '"<<fp.string().c_str()<<"'"<<std::endl;
                            inserted.first->second=fp; //update to overriding path
                        }
                    }
                    ++fileIt;
                }
            }
        }

        template <class ParserT, class CompletionAlg>
        struct ParseWorker
        {
            void operator()(const boost::shared_ptr<RepositoryLocal>& repository, const std::map<boost::filesystem::path, boost::filesystem::path>& paths)
            {
                ParseState state(repository);

                for (std::map<boost::filesystem::path, boost::filesystem::path>::const_iterator pathIt=paths.begin(); pathIt!=paths.end(); ++pathIt)
                {
                    boost::shared_ptr<boost::property_tree::ptree> pt=boost::make_shared<boost::property_tree::ptree>();
                    try
                    {
                        boost::property_tree::read_xml(pathIt->second.string(), *pt, boost::property_tree::xml_parser::no_comments);
                    }
                    catch (boost::property_tree::xml_parser_error& err) //cant catch as const-ref due to bug in early boost versions.
                    {
                        std::ostringstream ss;
                        ss<<err.message()<<". Line: "<<err.line();
                        throw ParseError("Invalid XML", ss.str(), pathIt->second.string(), 10);
                    }

                    try
                    {
                        state.currentPath=pathIt->second.string();
                        state.propertyTree=pt;
                        ParserT parser;
                        boost::property_tree::ptree::iterator ptIt=state.propertyTree->begin();
                        if (parser.Match(ptIt->first, state))
                        {
                            parser.Parse(ptIt->second, state);
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
        :m_result(boost::make_shared<RepositoryLocal>())
    {
        std::map<boost::filesystem::path, boost::filesystem::path> douFiles;
        std::map<boost::filesystem::path, boost::filesystem::path> domFiles;
        SelectFiles(roots, douFiles, domFiles);

        lllog(5)<<"Starting ParseJob #files (dou/dom/tot): "<<douFiles.size()<<"/"<<domFiles.size()<<"/"<<douFiles.size()+domFiles.size()<<std::endl;

        ParseWorker<DouParser, DouCompletionAlgorithm>()(m_result, douFiles);
        ParseWorker<DomParser, DomCompletionAlgorithm>()(m_result, domFiles);

        lllog(5)<<"ParseJob finished"<<std::endl;
    }
}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport
