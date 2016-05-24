/******************************************************************************
*
* Copyright Saab AB, 2016 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson / joel.ottosson@consoden.se
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
#include <vector>
#include <set>
#include <sstream>
#include <boost/filesystem.hpp>
#include <Safir/Utilities/Internal/ConfigReader.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>
#include <Safir/Dob/Typesystem/Serialization.h>
#include <Safir/Websocket/Typesystem/TypeHierarchy.h>

#include <Safir/Dob/Typesystem/ToolSupport/Serialization.h>

#include "Typesystem.h"

namespace sd = Safir::Dob;
namespace ts = Safir::Dob::Typesystem;
namespace sup = Safir::Dob::Typesystem::ToolSupport;

using namespace Safir::Websocket::Typesystem;

typedef boost::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository> Repository;

namespace Typesystem
{
    namespace
    {
        std::wstring Wstr(const char* str)
        {
            if (str)
                return ts::Utilities::ToWstring(str);
            else
                return L"";
        }

        Repository CreateRepository()
        {
            std::vector<boost::filesystem::path> directories;
            //directories.push_back("/home/joot/Dropbox/dev/DotsParser/dots_generated_conv");
            Safir::Utilities::Internal::ConfigReader reader;

            //get all dou directory strings
            std::vector<std::pair<std::string,std::string> > dirs = Safir::Utilities::Internal::ConfigHelper::GetDouDirectories(reader);

            for (std::vector<std::pair<std::string,std::string> >::const_iterator it = dirs.begin();
                 it != dirs.end(); ++it)
            {
                boost::filesystem::path douDirectory(it->second);

                if (!boost::filesystem::exists(douDirectory) || !boost::filesystem::is_directory(douDirectory))
                {
                    std::cout<<"dou_directory '"+douDirectory.string()+"' in typesystem.ini does not appear to be a directory"<<std::endl;
                    exit(22);
                }

                directories.push_back(douDirectory);
            }

            try
            {
                //localRepository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(directories);
                auto localRepository=Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(directories);
                return localRepository;
            }
            catch(const Safir::Dob::Typesystem::ToolSupport::ParseError& err)
            {
                std::cout<<"********** Parse Error **********************************************"<<std::endl;
                std::cout<<"* Label: "<<err.Label()<<std::endl;
                std::cout<<"* Descr: "<<err.Description()<<std::endl;
                std::cout<<"* File:  "<<err.File()<<std::endl;
                std::cout<<"* ErrId: "<<err.ErrorId()<<std::endl;
                std::cout<<"*********************************************************************"<<std::endl;
                throw err;
            }
        }

        void InsertEnums(const Repository& rep, TypeHierarchyPtr& h)
        {
            std::set<ts::TypeId> typeIds;
            rep->GetAllEnumTypeIds(typeIds);

            for (auto it = typeIds.begin(); it != typeIds.end(); ++it)
            {
                EnumTypePtr et=EnumType::Create();
                const sup::EnumDescription* ed=rep->GetEnum(*it);
                //et->Summary()=Wstr(ed->Summary());
                et->Name()=Wstr(ed->GetName());
                et->SourceFile()=Wstr(ed->FileName());
                for (int i=0; i<ed->GetNumberOfValues(); ++i)
                {
                    et->Values().push_back(Wstr(ed->GetValueName(i)));
                }

                h->Enums().push_back(et);
            }
        }

        ClassTypePtr ToClassType(const Repository& rep, const sup::ClassDescription* cd)
        {
            ClassTypePtr ct=ClassType::Create();

            //ct->Summary()=Wstr(cd->Summary());
            ct->Name()=Wstr(cd->GetName());
            ct->SourceFile()=Wstr(cd->FileName());

            for (int i=0; i<cd->GetNumberOfMembers(); ++i)
            {
                //create member
                const sup::MemberDescription* md=cd->GetMember(i);
                MemberPtr member=Member::Create();
                //member->Summary()=Wstr(md->Summary());
                member->Name()=Wstr(md->GetName());
                member->Type()=Wstr(sup::TypeUtilities::GetTypeName(rep.get(), md));
                switch(md->GetCollectionType())
                {
                case SingleValueCollectionType:
                    break;
                case ArrayCollectionType:
                    member->ArraySize()=md->GetArraySize();
                    break;
                case SequenceCollectionType:
                    member->ArraySize()=-1;
                    break;
                case DictionaryCollectionType:
                {
                    if (md->GetKeyType()==EnumerationMemberType)
                        member->DictionaryKeyType()=Wstr(sup::TypeUtilities::GetTypeName(rep.get(), md->GetKeyTypeId()));
                    else
                        member->DictionaryKeyType()=Wstr(sup::TypeUtilities::GetTypeName(md->GetKeyType()));
                }
                    break;
                }
                ct->Members().push_back(member);
            }

            for (int i=0; i<cd->GetNumberOfDescendants(); i++)
            {
                const sup::ClassDescription* des=cd->GetDescendant(i);
                auto desClassType=ToClassType(rep, des);
                ct->Descendants().push_back(desClassType);
            }

            return ct;
        }

        void InsertClasses(const Repository& rep, TypeHierarchyPtr& h)
        {
            const sup::ClassDescription* cd=rep->GetClass(ts::Object::ClassTypeId);
            h->RootClass()=ToClassType(rep, cd);
        }

        std::string TypeHierarchyAsJson()
        {
            auto rep=CreateRepository();

            TypeHierarchyPtr h=TypeHierarchy::Create();
            InsertEnums(rep, h);
            InsertClasses(rep, h);
            std::string json=ts::Utilities::ToUtf8(ts::Serialization::ToJson(h));
            return json;
        }
    }

    std::string GetTypeHierarchy()
    {
        static std::string jsonStr=TypeHierarchyAsJson();
        return jsonStr;
    }

}
