/* ****************************************************************************
*
* Copyright Saab AB, 2014 (http://safirsdkcore.com)
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

using System.Collections.Generic;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using Safir.Dob.Typesystem.Internal;

namespace Safir.Dob.Typesystem
{
    internal class GeneratedAssemblies
    {
        public static GeneratedAssemblies Instance
        {
            get
            {
                // Check that the instance is null
                if (m_instance == null)
                {
                    // Lock the object
                    lock (m_syncRoot)
                    {
                        // Check to make sure its null
                        if (m_instance == null)
                        {
                            m_instance = new GeneratedAssemblies();
                        }
                    }
                }

                // Return the non-null instance of Singleton
                return m_instance;
            }
        }

        private System.Reflection.Assembly EventHandler(object sender, System.ResolveEventArgs args)
        {
            string path;
            if (!m_libs.TryGetValue(args.Name, out path) || string.IsNullOrEmpty(path))
            {
                return null;
            }

            //check the full name of the assembly on disk
            var assName = System.Reflection.AssemblyName.GetAssemblyName(path);
            if (assName.FullName != args.Name)
            {
                throw new Safir.Dob.Typesystem.ConfigurationErrorException
                    ("Failed to load assembly " +
                     path +
                     " since its FullName is incorrect. Expected '" +
                     args.Name + "' but got '" + assName.FullName + "'");
            }

            return System.Reflection.Assembly.LoadFrom(path);
        }


        private GeneratedAssemblies()
        {
            System.IntPtr generatedLibraries;
            System.Int32 size;
            Kernel.DotsC_GeneratedLibraryListDeleter deleter;

            Kernel.DotsC_GetGeneratedLibraryList(out generatedLibraries,
                                                 out size,
                                                 out deleter);
            Kernel.DotsC_GeneratedLibrary lib;

            if (size == 0)
            {
                throw new Safir.Dob.Typesystem.ConfigurationErrorException("Failed to read information from typesystem.ini");
            }

            System.AppDomain.CurrentDomain.AssemblyResolve += EventHandler;

            for (int i = 0; i < size; ++i)
            {
                System.IntPtr iterator =
                    (System.IntPtr)(generatedLibraries.ToInt64() +
                                    i * Marshal.SizeOf(typeof(Kernel.DotsC_GeneratedLibrary)));
                lib = (Kernel.DotsC_GeneratedLibrary)Marshal.PtrToStructure(iterator, typeof(Kernel.DotsC_GeneratedLibrary));

                if (lib.library)
                {
                    //If there is a version specified in the ini file we load that, otherwise we go with the
                    //cmake auto-generated version.
                    string version = string.IsNullOrEmpty(lib.dotnetAssemblyVersion) ?
                        BuildInfo.Version : lib.dotnetAssemblyVersion;

                    string fullName = lib.dotnetAssemblyName +
                        ", Version=" + version +
                        ", Culture=neutral, PublicKeyToken=40df165d3a3cadbc";

                    //The version stuff above means that we can only load assemblies that are built with the
                    //same version of Safir SDK Core as we were built with, unless otherwise specified in
                    //typesystem.ini.
                    //The key stuff means that we can only load safir_generated assemblies that were signed
                    //with the dots_generated-dotnet.snk key file.

                    string path = "";
                    if (!string.IsNullOrEmpty(lib.dotnetAssemblyLocation))
                    {
                        path = System.IO.Path.Combine(lib.dotnetAssemblyLocation,
                                                      lib.dotnetAssemblyName + ".dll");
                    }

                    m_libs.Add(fullName, path);
                }
            }

            deleter(generatedLibraries,size);

            //Load all the assemblies that we need. If they are in the GAC or in the application
            //load path they will be loaded from there, but otherwise we will trigger the
            //event handler, to load it from the configured path.
            foreach (var ass in m_libs.Keys)
            {
                m_assemblies.Add(System.Reflection.Assembly.Load(ass));
            }
        }

        private System.Type LoadType(System.Int64 typeId)
        {
            string typeName = Operations.GetName(typeId);
            if (typeName == "Object")
            {
                return System.Type.GetType("Safir.Dob.Typesystem.Object", true);
            }
            else
            {
                //try assemblies loaded into the system first.
                var type = System.Type.GetType(typeName, false);
                if (type != null)
                {
                    return type;
                }

                //then try the ones we loaded through typesystem.ini
                foreach (Assembly ass in m_assemblies)
                {
                    type = ass.GetType(typeName, false);
                    if (type != null)
                    {
                        return type;
                    }
                }
            }

            throw new IllegalValueException("Couldn't find type " + typeName + " in any of the loaded assemblies!");
        }

        //this needs to be synchronized since it accesses and modifies the cache
        [MethodImpl(MethodImplOptions.Synchronized)]
        public System.Type GetType(System.Int64 typeId)
        {
            System.Type result;
            if (m_typeCache.TryGetValue(typeId, out result))
            {
                return result;
            }

            if (m_typeCache.Count > 300)
            {
                int which = m_random.Next(0,m_typeCache.Count);
                int i = 0;
                foreach(System.Int64 tid in m_typeCache.Keys)
                {
                    if (which == i)
                    {
                        m_typeCache.Remove(tid);
                        break;
                    }
                    ++i;
                }
            }

            result = LoadType(typeId);
            m_typeCache.Add(typeId,result);
            return result;
        }

        // Static, VOLATILE variable to store single instance
        private static volatile GeneratedAssemblies m_instance;

        // Static synchronization root object, for locking
        private static object m_syncRoot = new object();

        private System.Collections.Generic.Dictionary<string, string> m_libs =
            new System.Collections.Generic.Dictionary<string, string>();

        private Dictionary<System.Int64, System.Type> m_typeCache =
            new Dictionary<System.Int64, System.Type>();

        private List<Assembly> m_assemblies = new List<Assembly>();

        private System.Random m_random = new System.Random();
    }

}
