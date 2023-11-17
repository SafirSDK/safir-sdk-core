/******************************************************************************
*
* Copyright Saab AB, 2004-2023 (http://safirsdkcore.com)
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
#pragma once

#include <boost/filesystem.hpp>
#include <codecvt>
#include <memory>
#include <Safir/Dob/Typesystem/ToolSupport/Internal/TypeParserImpl.h>
#include <Safir/Dob/Typesystem/ToolSupport/ParseError.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeRepository.h>

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace ToolSupport
{
    /**
     * Extract all files from the given root folders separated as dou- and dom- files. The resulting file lists can
     * contain duplicates, i.e overrides that must be handled later on.
     *
     * @param roots [in] - A vector of root directories containing dou- and dom-files.
     * @param douFiles [out] - A vector where all dou-filepaths found in roots will be inserted.
     * @param domFiles [out] - A vector where all dom-filepaths found in roots will be inserted.
     * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
     */
    static inline void GetFilesFromRootDirectories(const std::vector<boost::filesystem::path>& roots,
                                         std::vector<boost::filesystem::path>& douFiles,
                                         std::vector<boost::filesystem::path>& domFiles)
    {
        std::set<boost::filesystem::path> thisRootFiles; // Overrides are only allowed between different roots. Duplicates within the same root are not allowed.

        for (const auto& root : roots)
        {
            thisRootFiles.clear();
            boost::filesystem::path rootDir = boost::filesystem::canonical(root).string(std::codecvt_utf8_utf16<wchar_t>());
            //Check paths are valid directories. We dont care about duplicates, that will only result in overriding with same file
            if (!boost::filesystem::exists(rootDir) || !boost::filesystem::is_directory(rootDir))
            {
                throw ParseError("Invalid directory path", "The specified root directory does not exist.", root.string(), 8);
            }

            boost::filesystem::recursive_directory_iterator fileIt(rootDir), fileItEnd;
            while (fileIt!=fileItEnd)
            {
                const boost::filesystem::path& fp=fileIt->path();

                if (thisRootFiles.find(fp.filename()) != thisRootFiles.end())
                {
                    std::ostringstream os;
                    os<<"The directory '"<<root.string()<<"' contains duplicated version of file '"<<fp.filename().string()<<"'"<<std::endl;
                    throw ParseError("Duplicated dou/dom file", os.str(), fp.string(), 2);
                }
                thisRootFiles.insert(fp.filename());

                if (fp.extension()==".dou")
                {
                    douFiles.push_back(fp);
                }
                else if (fp.extension()==".dom")
                {
                    domFiles.push_back(fp);
                }
                ++fileIt;
            }
        }
    }

    /**
     * Will validate and parse a complete set of dou- and dom-files. If no error occurs, the
     * result is returned.
     * This version takes a vector of paths and each path is traversed recursively includeing subfolders. If the same
     * dou-/dom- file exists at many paths the latest found will override the previous ones. I.e files found at paths
     * closer to the end of the vector will override files closer to the beginning of the vector.
     *
     * @param roots [in] - A vector of root directories containing dou- and dom-files that shall be parsed.
     * @return TypeRepository containing all types, i.e classes, exceptions, enums, properties and property mappings.
     * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
     */
    static inline std::shared_ptr<const TypeRepository> ParseTypeDefinitions(const std::vector<boost::filesystem::path>& roots)
    {
        std::vector<boost::filesystem::path> douFiles, domFiles;
        GetFilesFromRootDirectories(roots, douFiles, domFiles);
        return Internal::ParseTypeDefinitionsImpl(douFiles, domFiles);
     }

    /**
     * Will validate and parse a complete set of dou- and dom-files from If no error occurs, the
     * result is returned. The root path will be parsed recursively, i.e any subfolders will be parsed too.
     *
     * @param root [in] - Root directory path to location of dou- and dom-files that shall be parsed.
     * @return TypeRepository containing all types, i.e classes, exceptions, enums, properties and property mappings.
     * @throws Safir::Dob::Typesystem::Parser:ParseError The dou- or dom- files at the specified path contains errors.
     */
    static inline std::shared_ptr<const TypeRepository> ParseTypeDefinitions(const boost::filesystem::path& root)
    {
        std::vector<boost::filesystem::path> roots;
        roots.push_back(root);
        return ParseTypeDefinitions(roots);
    }



}
}
}
} //end namespace Safir::Dob::Typesystem::ToolSupport


