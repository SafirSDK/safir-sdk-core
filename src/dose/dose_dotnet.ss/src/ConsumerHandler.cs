/* ****************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
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
using System.Runtime.InteropServices;
namespace Safir.Dob
{
    /// <summary>
    /// Internal class for converting consumers to "pointers" that can be used in dose_dll.
    /// </summary>
    internal sealed class ConsumerHandler
    {
        private ConsumerHandler()
        {

        }

        // Property to retrieve the only instance of the Singleton
        public static ConsumerHandler Instance
        {
            get
            {
                // Check that the instance is null
                if (m_instance == null)
                {
                    // Lock the object
                    lock (m_instantiationLock)
                    {
                        // Check to make sure its null
                        if (m_instance == null)
                        {
                            m_instance = new ConsumerHandler();
                        }
                    }
                }

                // Return the non-null instance of Singleton
                return m_instance;
            }
        }

        static public Internal.ConsumerBase ToConsumer(System.IntPtr p)
        {
            return (Internal.ConsumerBase)GCHandle.FromIntPtr(p).Target;
        }

        //needs locking since it reads and writes the consumer table
        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
        public System.IntPtr AddReference(Internal.ConsumerBase consumer)
        {
            if (consumer == null)
            {
                return System.IntPtr.Zero;
            }

            Reference reference;
            if (!m_consumerTable.TryGetValue(consumer, out reference))
            {
                reference = new Reference(GCHandle.Alloc(consumer));
                m_consumerTable.Add(consumer, reference);
            }
            ++reference.references;
            return GCHandle.ToIntPtr(reference.handle);
            //return System.IntPtr.Zero;
        }

        //needs locking since it reads and writes the consumer table
        [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
        public void DropReference(Internal.ConsumerBase consumer)
        {
            if (consumer == null)
            {
                return;
            }

            Reference reference;
            if (!m_consumerTable.TryGetValue(consumer, out reference))
            {
                return;
            }
            --reference.references;
            if (reference.references == 0)
            {
                m_consumerTable.Remove(consumer);
                reference.handle.Free();
            }
        }

        internal class Reference
        {
            public Reference(GCHandle h) { handle = h; references = 0; }
            public GCHandle handle;
            public int references;
        };

        private System.Collections.Generic.Dictionary<Internal.ConsumerBase, Reference> m_consumerTable =
            new System.Collections.Generic.Dictionary<Internal.ConsumerBase, Reference>();


        // Static, VOLATILE variable to store single instance
        private static volatile ConsumerHandler m_instance;

        // Static synchronization root object, for locking
        private static object m_instantiationLock = new object();

    }

}
