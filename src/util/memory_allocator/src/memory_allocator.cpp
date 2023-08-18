/******************************************************************************
*
* Copyright Saab AB, 2023 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars.hagstrom@consoden.se
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
#include <Safir/Dob/Internal/SharedMemoryObject.h>
#include <boost/exception/diagnostic_information.hpp>
#include <iostream>

//disable warnings in boost
#if defined _MSC_VER
  #pragma warning (push)
  #pragma warning (disable : 4100)
  #pragma warning (disable : 4267)
#endif

#include <boost/program_options.hpp>

#if defined _MSC_VER
  #pragma warning (pop)
#endif

std::wostream& operator<<(std::wostream& out, const std::string& str)
{
    return out << str.c_str();
}

std::wostream& operator<<(std::wostream& out, const boost::program_options::options_description& opt)
{
    std::ostringstream ostr;
    ostr << opt;
    return out << ostr.str().c_str();
}

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        options_description options("Options");
        options.add_options()
            ("help,h", "show help message")
            ("allocate,a",
             value<std::string>(&allocationLevelString),
             "Allocate shared memory until memory level reaches the specified level.")
            ("deallocate,d",
             "Deallocate all memory allocated by previous runs of this program.")
            ("show,s",
             "Show current memory allocation level");

        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcerr << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(options);
            return;
        }

        allocate = vm.count("allocate") != 0;
        deallocate = vm.count("deallocate") != 0;
        show = vm.count("show") != 0;

        if (vm.count("allocate") + vm.count("deallocate") + vm.count("show") != 1)
        {
            ShowHelp(options);
            return;
        }

        if (allocate)
        {
            try
            {
                allocationLevel = Safir::Dob::MemoryLevel::ToValue
                    (Safir::Dob::Typesystem::Utilities::ToWstring(allocationLevelString));
            }
            catch(const Safir::Dob::Typesystem::IllegalValueException&)
            {
                std::wcout << "'" << allocationLevelString << "' is not a valid value of Safir.Dob.MemoryLevel. "
                           << "Valid values are:\n";
                for (int val = Safir::Dob::MemoryLevel::First(); val <= Safir::Dob::MemoryLevel::Last(); ++val)
                {
                    std::wcout << "  "
                               << Safir::Dob::MemoryLevel::ToString(static_cast<Safir::Dob::MemoryLevel::Enumeration>(val))
                               << std::endl;
                }
                ShowHelp(options);
                return;
            }
        }

        parseOk = true;
    }
    bool parseOk;

    bool show;
    bool allocate;
    bool deallocate;
    Safir::Dob::MemoryLevel::Enumeration allocationLevel;
private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::wcout << std::boolalpha
                  << "Usage: control [OPTIONS]\n"
                  << desc
                  << std::endl;
    }

    std::string allocationLevelString;
};


class Allocator
    : public Safir::Dob::Internal::SharedMemoryObject
{
public:
    void AllocateUntilLevel(const Safir::Dob::MemoryLevel::Enumeration expectedLevel)
    {
        auto instance = GetSharedMemory().
            find_or_construct<Containers<ShmString>::vector>("SAFIR_MEMORY_ALLOCATOR_DATA")();

        while (GetMemoryLevel() < expectedLevel) //Not the fuzzy variant
        {
            instance->push_back(ShmString(32*1024, 'a'));
        }
    }

    void Deallocate()
    {
        GetSharedMemory().destroy<Containers<ShmString>::vector>("SAFIR_MEMORY_ALLOCATOR_DATA");
    }

    void ShowLevel()
    {
        const auto level = GetMemoryLevelFuzzy();
        std::wcout << "Current memory level is '"
                   << Safir::Dob::MemoryLevel::ToString(level)
                   << "'" << std::endl;
    }
};



int main(int argc, char * argv[])
{
    try
    {
        const ProgramOptions options(argc, argv);
        if (!options.parseOk)
        {
            return 1;
        }

        Allocator app;
        if (options.show)
        {
            app.ShowLevel();
        }
        else if (options.allocate)
        {
            app.AllocateUntilLevel(options.allocationLevel);
            app.ShowLevel();
        }
        else if (options.deallocate)
        {
            app.Deallocate();
            app.ShowLevel();
        }

        return 0;
    }
    catch (...)
    {
        std::wcout << "Caught exception: " << boost::current_exception_diagnostic_information().c_str() << std::endl;
        return 1;
    }
}
