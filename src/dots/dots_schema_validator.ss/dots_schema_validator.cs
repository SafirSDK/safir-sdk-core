/******************************************************************************
*
* Copyright Saab AB, 2006-2008 (http://www.safirsdk.com)
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
using System.Xml;
using System.Xml.Schema;
using System.IO;

namespace Dots
{
    class Validator
    {
        static private XmlSchemaSet schemas;
        static private string currentFile;
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static int Main(string[] args)
        {
            if (args.Length == 0 || args[0] == "-h" || args[0] == "-?" || args[0] == "--help")
            {
                System.Console.WriteLine("Give files as arguments to verify them (wildcards don't work)");
                System.Console.WriteLine("Give a directory as argument to recursively verify all dou and dom files in it");
                return 1;
            }
            
            string schemaPath = Environment.GetEnvironmentVariable("SAFIR_RUNTIME") + Path.DirectorySeparatorChar +
                "data" + Path.DirectorySeparatorChar + 
                "text" + Path.DirectorySeparatorChar + 
                "dots" + Path.DirectorySeparatorChar +
                "config" + Path.DirectorySeparatorChar +
                "dots_unit.xsd";

            System.Console.WriteLine("Verifying dou, dom and xml files using xml schema " + schemaPath);
            schemas = new XmlSchemaSet();
            try
            {
                schemas.Add("urn:safir-dots-unit", schemaPath);
            }
            catch(Exception e)
            {
                System.Console.WriteLine("Could not read schema file " + schemaPath + ".\nMake sure it is there and that it is a valid schema file");
                System.Console.WriteLine("Exception message: " + e.Message);
                return 1;
            }
            
            try
            {
                foreach (string argument in args)
                {
                    if (Directory.Exists(argument))
                    {
                        ValidateDirectory(argument);
                    }
                    else if (File.Exists(argument))
                    {
                        Validate(argument);
                    }
                    else
                    {
                        System.Console.WriteLine("Unrecognized argument '" + argument + "'");
                    }
                }
            }
            catch (System.Xml.XmlException e)
            {
                System.Console.WriteLine();
                System.Console.WriteLine(currentFile + ":" + e.LineNumber +":" + e.LinePosition + ": " + e.Message);
                return 1;
            }
            return 0;
        }

        public static void ValidateDirectory(string path)
        {
            foreach (string filename in Directory.GetFiles(path,"*.dou"))
            {
                Validate(filename);
            }
            foreach (string filename in Directory.GetFiles(path,"*.dom"))
            {
                Validate(filename);
            }
            foreach (string filename in Directory.GetFiles(path, "*.xml"))
            {
                Validate(filename);
            }

            foreach (string filename in Directory.GetDirectories(path))
            {
                ValidateDirectory(filename);
            }
        }

        public static void Validate(string filename)
        {
            currentFile = filename;
            XmlReaderSettings settings = new XmlReaderSettings();
            settings.ValidationType = ValidationType.Schema;
            settings.Schemas = schemas;
            settings.ValidationEventHandler += new ValidationEventHandler (reader_ValidationEventHandler);

            // Create the XmlReader object.
            XmlReader reader = XmlReader.Create(filename, settings);
            
            // Parse the file. 
            while (reader.Read())
            {

            }
        }

        public static void reader_ValidationEventHandler(object sender, ValidationEventArgs e)
        {
            System.Console.WriteLine();
            System.Console.WriteLine(currentFile + ":" + e.Exception.LineNumber +":" + e.Exception.LinePosition + ": " + e.Message);
            Environment.Exit(1);
        }
    }
}
