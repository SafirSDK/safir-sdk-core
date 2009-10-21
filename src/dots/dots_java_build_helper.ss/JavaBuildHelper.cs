/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
*
* Created by: Lars Hagstrom / stlrha
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
using System.Collections.Generic;
using System.Text;

namespace dots_java_build_helper
{
    class JavaBuildHelper
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static int Main(string[] args)
        {
            if (args.Length == 0 || args[0] == "-h" || args[0] == "-?" || args[0] == "--help")
            {
                System.Console.WriteLine("Give files as arguments to add them to the java_build file");
                System.Console.WriteLine("Give a directory as argument to recursively add all files in it to the java_build file");
                return 1;
            }
            const string buildFileName = "java_build.txt";

            if (System.IO.File.Exists(buildFileName))
            {
                System.IO.File.Delete(buildFileName);
            }
            System.IO.FileStream fs = System.IO.File.Create(buildFileName);



            foreach (string argument in args)
            {
                if (System.IO.Directory.Exists(argument))
                {
                    RecursivelyAddDirectory(argument,fs);
                }
                else if (System.IO.File.Exists(argument))
                {
                    AddFile(argument,fs);
                }
                else
                {
                    System.Console.WriteLine("Unrecognized argument '" + argument + "'");
                }
            }
            
            return 0;
        }

        public static void RecursivelyAddDirectory(string path, System.IO.FileStream fs)
        {
            foreach (string filename in System.IO.Directory.GetFiles(path, "*.java"))
            {
                AddFile(filename,fs);
            }

            foreach (string filename in System.IO.Directory.GetDirectories(path))
            {
                RecursivelyAddDirectory(filename,fs);
            }
        }

        public static void AddFile(string filename, System.IO.FileStream fs)
        {
            filename += "\n";
            byte[] info = new UTF8Encoding(true).GetBytes(filename);
            fs.Write(info, 0, info.Length);
        }

   
    
    }
}
