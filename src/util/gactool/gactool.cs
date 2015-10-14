/* ****************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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

using System;
//using System.Collections.Generic;
using System.Text;
using System.IO;
//using System.Security;
//using System.EnterpriseServices;

/// <summary>
/// This program allows you to add and remove assemblies from the GAC
/// </summary>
class gactool
{
    private void InstallAssembly(string path)
    {
        var loadedAssembly = System.Reflection.Assembly.LoadFile(path);

        if (loadedAssembly.GetName().GetPublicKey().Length == 0)
        {
            throw new Exception("The assembly '" + path + "' must be strongly signed.");
        }

        m_publisher.GacInstall(path);
        Console.WriteLine("Installed assembly '" + path + "' into the GAC.");
    }

    private void UninstallAssembly(string path)
    {
        var loadedAssembly = System.Reflection.Assembly.LoadFile(path);

        if (loadedAssembly.GetName().GetPublicKey().Length == 0)
        {
            throw new Exception("The assembly '" + path + "' must be strongly signed.");
        }

        m_publisher.GacRemove(path);
        Console.WriteLine("Removed assembly '" + path + "' from the GAC.");
    }

    private void Run(string[] args)
    {
        Console.OutputEncoding = Encoding.GetEncoding(Encoding.Default.CodePage);
        if (args.Length != 2 || (args[0] != "--install" && args[0] != "--uninstall"))
        {
            Console.WriteLine("usage: gactool --install/--uninstall <path>");
            Console.WriteLine("       <path> can be either relative or absolute path to assembly\n       or to a directory containing assemblies");
            return;
        }

        bool install = args[0] == "--install";

        var path = Path.GetFullPath(args[1]);
        Console.WriteLine("Path is " + Path.GetFullPath(path));

        if (File.Exists(path))
        {
            if (install)
            {
                InstallAssembly(path);
            }
            else
            {
                UninstallAssembly(path);
            }
        }
        else if (Directory.Exists(path))
        {
            foreach (string file in Directory.GetFiles(path, "*.dll"))
            {
                if (install)
                {
                    InstallAssembly(file);
                }
                else
                {
                    UninstallAssembly(file);
                }
            }
        }
        else
        {
            throw new Exception("No such file or directory");
        }
    }

    static int Main(string[] args)
    {
        try
        {
            new gactool().Run(args);
        }
        catch(Exception e)
        {
            Console.WriteLine("gactool failed: " + e.Message);
            return 1;
        }
        return 0;
    }

    System.EnterpriseServices.Internal.Publish m_publisher = new System.EnterpriseServices.Internal.Publish();
}
