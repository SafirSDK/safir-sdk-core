/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
* 
* Created by: Lars Hagström / stlrha
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

namespace dots_cpp_build_helper
{
    class CppBuildHelper
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static int Main(string[] args)
        {
            if (args.Length == 0 || args[0] == "-h" || args[0] == "-?" || args[0] == "--help")
            {
                System.Console.WriteLine("Give files as arguments to add them to the cpp_build file");
                System.Console.WriteLine("Give a directory as argument to recursively add all files in it to the cpp_build file");
                return 1;
            }
            const string buildFileName = "cpp_build.txt";

            if (System.IO.File.Exists(buildFileName))
            {
                System.IO.File.Delete(buildFileName);
            }

            using (System.IO.FileStream fs = System.IO.File.Create(buildFileName))
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(fs, Encoding.UTF8))
            {

                foreach (string argument in args)
                {
                    if (System.IO.Directory.Exists(argument))
                    {
                        RecursivelyAddDirectory(argument, sw);
                    }
                    else if (System.IO.File.Exists(argument))
                    {
                        AddFile(argument, sw);
                    }
                    else
                    {
                        System.Console.WriteLine("Unrecognized argument '" + argument + "'");
                    }
                }
            }

            // Dirty fix to generate a proper gpr file for dots_generated
            GenerateDotsGeneratedGprFile();
            return 0;
        }

        public static void RecursivelyAddDirectory(string path, System.IO.StreamWriter sw)
        {
            foreach (string filename in System.IO.Directory.GetFiles(path, "*.cpp"))
            {
                AddFile(filename,sw);
            }

            foreach (string filename in System.IO.Directory.GetDirectories(path))
            {
                RecursivelyAddDirectory(filename,sw);
            }
        }

        public static void AddFile(string filename, System.IO.StreamWriter sw)
        {
            //filename += "\n";
            //byte[] info = new UTF8Encoding(true).GetBytes(filename);
            sw.WriteLine("." + filename);
                //Write(info, 0, info.Length);
        }

        public static void GenerateDotsGeneratedGprFile()
        {
            string[] filenames = System.IO.Directory.GetFiles("..\\ada", "*.ads");
            if (filenames.Length == 0)
            {
                return;
            }
            string firstUnit = filenames[0].Replace("..\\ada\\", "");
            firstUnit = firstUnit.Replace(".ads", "");
            firstUnit = firstUnit.Replace("-", ".");

            using (System.IO.FileStream fs = System.IO.File.Create("../dots_generated_ada_library.gpr"))
            using (System.IO.StreamWriter sw = new System.IO.StreamWriter(fs))
            {
                sw.WriteLine("with \"safir_defaults.gpr\";");
                sw.WriteLine("with \"dots_ada.gpr\";");
                sw.WriteLine("");
                sw.WriteLine("project Dots_Generated_Ada_Library is");
                sw.WriteLine("\tfor Source_Dirs use (\"ada\");");
                sw.WriteLine("\tfor Object_Dir use \"ada\\obj\";");
                sw.WriteLine("\tfor Library_Dir use \"ada\\lib\";");
                sw.WriteLine("\tfor Library_ALI_Dir use \"ada\\ali\";");
                sw.WriteLine("\tfor Library_Src_Dir use \"ada\\interface\";");
                sw.WriteLine("\tfor Library_Name use \"dots_generated_ada\";");

                sw.Write("\tfor Library_Interface use (");
                sw.Write("\"" + firstUnit + "\"");
                for (int i = 1; i < filenames.Length; i++)
                {
                    string unit = filenames[i].Replace("..\\ada\\", "");
                    unit = unit.Replace(".ads", "");
                    unit = unit.Replace("-", ".");
                    sw.Write(",\"" + unit + "\"");
                }
                sw.Write(");");
                sw.WriteLine("");
                sw.WriteLine("\tfor Library_Kind use \"dynamic\";");
                sw.WriteLine("\tpackage Compiler is");
                sw.WriteLine("\t\tfor Default_Switches (\"ada\") use Safir_Defaults.Compiler'Default_Switches (\"Ada\");");
                sw.WriteLine("\tend Compiler;");
                sw.WriteLine("end Dots_Generated_Ada_Library;");
            }
        }
    
    }
}
