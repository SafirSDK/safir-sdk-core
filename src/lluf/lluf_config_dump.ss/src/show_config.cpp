/******************************************************************************
*
* Copyright Saab AB, 2013 (http://www.safirsdk.com)
*
* Created by: Anders Widén <anders.widen@consoden.se>
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

#include <Safir/Utilities/Internal/ConfigReader.h>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable:4100)
#endif

#include <boost/program_options.hpp>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

class ProgramOptions
{
public:
    ProgramOptions(int argc, char* argv[])
        : parseOk(false)
    {
        using namespace boost::program_options;
        Safir::Utilities::Internal::ConfigReader reader;
        
        options_description general("General Options");
        general.add_options()
            ("help,h", "show help message");

        options_description all_options;
        all_options.add(general);
        
        variables_map vm;

        try
        {
            store(command_line_parser(argc, argv).
                  options(all_options).run(), vm);
            notify(vm);
        }
        catch (const std::exception& exc)
        {
            std::wcout << "Error parsing command line: " << exc.what() << "\n" << std::endl;
            ShowHelp(all_options);
            return;
        }

        if (vm.count("help"))
        {
            ShowHelp(all_options);
            return;
        }

        parseOk = true;
    }
    bool parseOk;

private:
    static void ShowHelp(const boost::program_options::options_description& desc)
    {
        std::cout << std::boolalpha
                   << "Writes the Safir SDK ini file configuration to standard output.\n"
                   << desc << "\n"
                   << std::endl;
    }

};

static void PrintPTree (const boost::property_tree::ptree& ptree)
{
    for (boost::property_tree::ptree::const_iterator it = ptree.begin();
         it != ptree.end(); ++it)
    {
        const bool isSection = !it->second.empty();

        if (isSection)
        {
            std::cout << '[' << it->first << ']' << std::endl;
            PrintPTree(it->second);
        }
        else
        {
            std::cout << it->first << '=' << it->second.get_value<std::string>() << std::endl;
        }
    }
}


int main(int argc, char * argv[])
{
    const ProgramOptions options(argc,argv);

    if (!options.parseOk)
    {
        return 1;
    }

    try
    {
        Safir::Utilities::Internal::ConfigReader reader;

        PrintPTree(reader.Locations());
        PrintPTree(reader.Logging());
        PrintPTree(reader.Typesystem());
    }
    catch (const std::exception&)
    {
        std::cout << "Can't read configuration. " << std::endl;
        return 1;
    }
    return 0;
}
