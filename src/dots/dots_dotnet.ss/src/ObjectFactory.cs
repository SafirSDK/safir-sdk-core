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

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// This class is an object factory for all automatically generated DOB classes.
    /// <para/>
    /// Each generated class automatically registers itself with this class.
    /// <para/>
    /// Users can call the CreateObject(TypeId) routine to create objects of a desired type
    /// (this is if they receive the type id from some other application so that they cannot
    /// call the Create routine of the class itself directly).
    /// </summary>
    public class ObjectFactory
    {
        /// <summary>
        /// Property to retrieve the only instance of the Singleton
        /// </summary>
        public static ObjectFactory Instance
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
                            m_instance = new ObjectFactory();
                        }
                    }
                }

                // Return the non-null instance of Singleton
                return m_instance;
            }
        }

        /// <summary>
        /// Create a new object from a blob.
        /// <para/>
        /// This method takes a blob and extracts the typeId from it and then calls the
        /// appropriate callback to create the object.
        /// </summary>
        /// <param name="blob">The blob to deserialize.</param>
        /// <returns>A created Object.</returns>
        /// <exception cref="IllegalValueException">If the type represented by the blob isn't found in the ObjectFactory.</exception>
        public Object CreateObject(System.IntPtr blob)
        {
            if (blob == System.IntPtr.Zero)
            {
                throw new SoftwareViolationException("CreateObject got NULL blob!");
            }

            System.Int64 typeId = Internal.Kernel.DotsC_GetTypeId(blob);
            string typeName = Operations.GetName(typeId);
            if (typeName == "Object")
            {
                typeName = "Safir.Dob.Typesystem.Object";
            }
            else
            {
                typeName += ", dots_generated-dotnet";
            }

            System.Type type = System.Type.GetType(typeName);
            if (type == null)
            {
                throw new IllegalValueException("Could not create type " + typeName);
            }
            Object obj = System.Activator.CreateInstance(type, blob) as Object;
            if (obj == null)
            {
                throw new IllegalValueException("Could not create type " + typeName);
            }
            return obj;
        }
        
        /// <summary>
        /// Create a new "empty" object from a typeid.
        /// <para/>
        /// This method takes a TypeId and calls the appropriate callback to create
        /// an object of the desired type.
        /// </summary>
        /// <param name="typeId">The TypeId of the object to create.</param>
        /// <returns>A created Object.</returns>
        /// <exception cref="IllegalValueException">The type couldn't be found in the ObjectFactory.</exception>
        public Object CreateObject(System.Int64 typeId)
        {
            string typeName = Operations.GetName(typeId);
            if (typeName == "Object")
            {
                typeName = "Safir.Dob.Typesystem.Object";
            }
            else
            {
                typeName += ", dots_generated-dotnet";
            }
            System.Type type = System.Type.GetType(typeName ,true);
            if (type == null)
            {
                throw new IllegalValueException("Could not create type " + typeName);
            }
            Object obj = System.Activator.CreateInstance(type) as Object;
            if (obj == null)
            {
                throw new IllegalValueException("Could not create type " + typeName);
            }
            return obj;
        }

        // Static, VOLATILE variable to store single instance
        private static volatile ObjectFactory m_instance;

        // Static synchronization root object, for locking
        private static object m_syncRoot = new object();

    }
        
}
