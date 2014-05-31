/* ****************************************************************************
*
* Copyright Saab AB, 2014 (http://safir.sourceforge.net)
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

        private GeneratedAssemblies()
        {
            m_assemblies.Add(System.Reflection.Assembly.LoadFile("/home/lars/logan/src/dots/dots_test_dou.ss/dots_generated-DotsTest-dotnet.dll"));
            m_assemblies.Add(System.Reflection.Assembly.LoadFile("/home/lars/logan/src/dots/dots_test_extra_dou.ss/dots_generated-DotsTestExtra-dotnet.dll"));
            //TODO
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
                foreach (Assembly ass in m_assemblies)
                {
                    System.Type t = ass.GetType(typeName, false);
                    if (t != null)
                    {
                        return t;
                    }
                }
            }

            throw new IllegalValueException("Could find type " + typeName + " in any of the loaded assemblies!");
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

        private Dictionary<System.Int64, System.Type> m_typeCache =
            new Dictionary<System.Int64, System.Type>();
        
        private List<Assembly> m_assemblies = new List<Assembly>();

        private System.Random m_random = new System.Random();
    }
        
}
