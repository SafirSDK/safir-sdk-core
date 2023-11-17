/******************************************************************************
*
* Copyright Saab AB, 2004-2023 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include <iostream>
#include <boost/filesystem.hpp>
#include "../src/dou_diff_helper.h"

#include <Safir/Dob/Typesystem/ToolSupport/TypeParser.h>
#include <Safir/Dob/Typesystem/ToolSupport/TypeUtilities.h>

namespace fs = boost::filesystem;

DouDiffHelper::NameTypeVector GetMembers(int64_t typeId, const std::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository>& repository);
DouDiffHelper::NameTypeVector GetParameters(int64_t typeId, const std::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository>& repository);
bool Check(DouDiffHelper::NameTypeVector result, DouDiffHelper::NameTypeVector expected);

int main(int argc, char* argv[])
{
    int returnCode = 0;

    if (argc<2)
    {
        std::cout<<"Too few arguments!"<<std::endl;
        std::cout<<"Usage: 'dots_configuration_checkl_test <test_dou_dir>'"<<std::endl;
        return 1;
    }

    fs::path douDir(argv[1]);
    std::cout << "Start dots_configuration_check_test: " << douDir.string() << std::endl;

    int64_t baseMyEntityChecksum = -9111722957543779982LL;

    // ---------------------------------------
    // no_matching_checksum
    // ---------------------------------------
    {
        std::cout << "\n=== Start: no_matching_checksum ===" << std::endl;
        std::vector<fs::path> roots = {
            douDir / fs::path("no_matching_checksum") / fs::path("base"),
            douDir / fs::path("no_matching_checksum") / fs::path("override")
        };

        DouDiffHelper dh(roots);
        if (dh.LoadType("Test.MyEntity", 123LL))
        {
            std::cout << "Test failed!" << std::endl;
            returnCode = -1;
        }
        else
        {
            std::cout << "Test passed! " << std::endl;
        }
    }

    // ---------------------------------------
    // missing_elements
    // ---------------------------------------
    {
        std::cout << "\n=== Start: missing_elements ===" << std::endl;
        auto failed = false;
        std::vector<fs::path> roots = {
            douDir / fs::path("missing_elements") / fs::path("base"),
            douDir / fs::path("missing_elements") / fs::path("override")
        };

        DouDiffHelper dh(roots);
        if (dh.LoadType("Test.MyEntity", baseMyEntityChecksum))
        {
            auto typeId = LlufId_Generate64("Test.MyEntity");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);

            const auto& diff = dh.DiffLoadedType(members, parameters);

            if (!diff.hasDiff)
            {
                std::cout << "missing_elements failed - diff flag not set." << std::endl;
                failed = true;
            }

            if (!diff.addedMembers.empty() || !diff.addedParameters.empty())
            {
                std::cout << "missing_elements failed - unexpected added elements." << std::endl;
                failed = true;
            }

            if (!Check(diff.missingMembers, {{"c", "String"}}))
            {
                std::cout << "missing_elements failed - missingMembers not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.missingParameters, {{"a", "Int32"}}))
            {
                std::cout << "missing_elements failed - missingParameters not correct." << std::endl;
                failed = true;
            }

            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "missing_elements failed - couldn't find dou-file." << std::endl;
            failed = true;
        }

        returnCode = failed ? -1 : returnCode;
        std::cout<< "Test " << (failed ? "failed!" : "passed!") << std::endl;
    }

    // ---------------------------------------
    // added_elements
    // ---------------------------------------
    {
        std::cout << "\n=== Start: added_elements ===" << std::endl;
        auto failed = false;
        std::vector<fs::path> roots = {
            douDir / fs::path("added_elements") / fs::path("base"),
            douDir / fs::path("added_elements") / fs::path("override")
        };

        DouDiffHelper dh(roots);
        if (dh.LoadType("Test.MyEntity", baseMyEntityChecksum))
        {
            auto typeId = LlufId_Generate64("Test.MyEntity");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);

            const auto& diff = dh.DiffLoadedType(members, parameters);

            if (!diff.hasDiff)
            {
                std::cout << "added_elements failed - diff flag not set." << std::endl;
                failed = true;
            }

            if (!diff.missingMembers.empty() || !diff.missingParameters.empty())
            {
                std::cout << "added_elements failed - unexpected missing elements." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedMembers, {{"E", "EntityId"}}))
            {
                std::cout << "added_elements failed - addedMembers not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedParameters, {{"F", "InstanceId"}}))
            {
                std::cout << "added_elements failed - addedParameters not correct." << std::endl;
                failed = true;
            }

            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "added_elements failed - couldn't find dou-file." << std::endl;
            failed = true;
        }

        returnCode = failed ? -1 : returnCode;
        std::cout<< "Test " << (failed ? "failed!" : "passed!") << std::endl;
    }

    // ---------------------------------------
    // mixed_advanced
    // ---------------------------------------
    {
        std::cout << "\n=== Start: mixed_advanced ===" << std::endl;
        auto failed = false;
        std::vector<fs::path> roots = {
            douDir / fs::path("mixed_advanced") / fs::path("base"),
            douDir / fs::path("mixed_advanced") / fs::path("override")
        };

        DouDiffHelper dh(roots);

        // Safir.Dob.Entity
        std::cout << "--- Type: Safir.Dob.Entity ---" << std::endl;
        if (dh.LoadType("Safir.Dob.Entity", 0))
        {
            auto typeId = LlufId_Generate64("Safir.Dob.Entity");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);
            const auto& diff = dh.DiffLoadedType(members, parameters);
            if (diff.hasDiff)
            {
                std::cout << "mixed_advanced failed - diff flag was set for Safir.Dob.Entity" << std::endl;
                failed = true;
            }
            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "mixed_advanced failed - couldn't find dou-file for Safir.Dob.Entity" << std::endl;
            failed = true;
        }

        // Test.MyEntity
        std::cout << "--- Type: Test.MyEntity ---" << std::endl;
        if (dh.LoadType("Test.MyEntity", baseMyEntityChecksum))
        {
            auto typeId = LlufId_Generate64("Test.MyEntity");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);

            const auto& diff = dh.DiffLoadedType(members, parameters);


            if (!Check(diff.missingMembers, {{"d", "Int64"}}))
            {
                std::cout << "mixed_advanced failed - missingMembers not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.missingParameters, {{"b", "Test.MyEntity"}}))
            {
                std::cout << "mixed_advanced failed - missingParameters not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedMembers, {{"e", "Int64"}}))
            {
                std::cout << "mixed_advanced failed - addedMembers not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedParameters, {{"f", "Test.MyEntity"}}))
            {
                std::cout << "mixed_advanced failed - addedParameters not correct." << std::endl;
                failed = true;
            }

            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "mixed_advanced failed - couldn't find dou-file for Test.MyEntity" << std::endl;
            failed = true;
        }

        // Test.MyEntityInherited
        std::cout << "--- Type: Test.MyEntityInherited ---" << std::endl;
        if (dh.LoadType("Test.MyEntityInherited", 7353289459398808221))
        {
            auto typeId = LlufId_Generate64("Test.MyEntityInherited");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);

            const auto& diff = dh.DiffLoadedType(members, parameters);


            if (!diff.addedParameters.empty())
            {
                std::cout << "mixed_advanced failed - addedParameters not correct." << std::endl;
                failed = true;
            }

            if (!diff.missingMembers.empty())
            {
                std::cout << "mixed_advanced failed - missingMembers not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.missingParameters, {{"StrPar", "String"}}))
            {
                std::cout << "mixed_advanced failed - missingParameters not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedMembers, {{"IntMem1", "Int32"}, {"IntMem2", "Int32"}, {"IntMem3", "Int32"}}))
            {
                std::cout << "mixed_advanced failed - addedMembers not correct." << std::endl;
                failed = true;
            }

            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "mixed_advanced failed - couldn't find dou-file for Test.MyEntityInherited" << std::endl;
            failed = true;
        }

        // Test.MyOtherEntity
        std::cout << "--- Type: Test.MyOtherEntity ---" << std::endl;
        if (dh.LoadType("Test.MyOtherEntity", -8074543905675095924))
        {
            auto typeId = LlufId_Generate64("Test.MyOtherEntity");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);

            const auto& diff = dh.DiffLoadedType(members, parameters);

            if (!diff.addedParameters.empty())
            {
                std::cout << "mixed_advanced failed - addedParameters not correct." << std::endl;
                failed = true;
            }
            if (!diff.missingParameters.empty())
            {
                std::cout << "mixed_advanced failed - missingParameters not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.missingMembers, {{"Foo", "String"}}))
            {
                std::cout << "mixed_advanced failed - missingParameters not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedMembers, {{"AlphaBeta", "String"}}))
            {
                std::cout << "mixed_advanced failed - addedMembers not correct." << std::endl;
                failed = true;
            }

            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "mixed_advanced failed - couldn't find dou-file for Test.MyOtherEntity" << std::endl;
            failed = true;
        }

        // Test.Params
        std::cout << "--- Type: Test.Params ---" << std::endl;
        if (dh.LoadType("Test.Params", 2224062794534087353))
        {
            auto typeId = LlufId_Generate64("Test.Params");
            auto repo = Safir::Dob::Typesystem::ToolSupport::ParseTypeDefinitions(roots);
            auto members = GetMembers(typeId, repo);
            auto parameters = GetParameters(typeId, repo);

            const auto& diff = dh.DiffLoadedType(members, parameters);

            if (!diff.addedMembers.empty())
            {
                std::cout << "mixed_advanced failed - addedMembers not correct." << std::endl;
                failed = true;
            }
            if (!diff.missingMembers.empty())
            {
                std::cout << "mixed_advanced failed - missingMembers not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.addedParameters, {{"EntPar", "Test.MyEntity"}}))
            {
                std::cout << "mixed_advanced failed - missingParameters not correct." << std::endl;
                failed = true;
            }

            if (!Check(diff.missingParameters, {{"ObjPar", "Test.MyEntity"}}))
            {
                std::cout << "mixed_advanced failed - addedMembers not correct." << std::endl;
                failed = true;
            }

            std::cout << "Diff: " << diff << std::endl;
        }
        else
        {
            std::cout << "mixed_advanced failed - couldn't find dou-file for Test.Params" << std::endl;
            failed = true;
        }

        returnCode = failed ? -1 : returnCode;
        std::cout<< "Test " << (failed ? "failed!" : "passed!") << std::endl;
    }

    return returnCode;
}

DouDiffHelper::NameTypeVector GetMembers(int64_t typeId, const std::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository>& repository)
{
    DouDiffHelper::NameTypeVector members;
    auto cd = repository->GetClass(typeId);
    for (int i = cd->GetNumberOfInheritedMembers(); i < cd->GetNumberOfMembers(); ++i)
    {
        auto md = cd->GetMember(i);
        std::string name = md->GetName();
        std::string type = Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(repository.get(), md);
        members.emplace_back(name, type);
    }
    return members;
}

DouDiffHelper::NameTypeVector GetParameters(int64_t typeId, const std::shared_ptr<const Safir::Dob::Typesystem::ToolSupport::TypeRepository>& repository)
{
    DouDiffHelper::NameTypeVector parameters;
    auto cd = repository->GetClass(typeId);
    for (int i = cd->GetNumberOfInheritedParameters(); i < cd->GetNumberOfParameters(); ++i)
    {
        auto pd = cd->GetParameter(i);
        std::string name = pd->GetName();
        std::string type = Safir::Dob::Typesystem::ToolSupport::TypeUtilities::GetTypeName(repository.get(), pd);
        parameters.emplace_back(name, type);
    }
    return parameters;
}

bool Check(DouDiffHelper::NameTypeVector result, DouDiffHelper::NameTypeVector expected)
{
    if (result.size() != expected.size())
    {
        std::cout << "Sizes doesn't match!" << std::endl;
        return false;
    }

    bool ok = true;
    for (const auto& e : expected)
    {
        auto it = std::find_if(result.begin(), result.end(), [&e](const auto& r)
        {
            return r.first == e.first && r.second == e.second;
        });
        if (it == result.end())
        {
            std::cout << "Couldn't find expected element: '" << e.second << " : " << e.first << "'" << std::endl;
            ok = false;
        }
    }

    return ok;
}
