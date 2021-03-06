/* ****************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safirsdkcore.com)
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

using System.Runtime.InteropServices;

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// Contains methods used when passing exceptions across language boundaries.
    /// </summary>
    public class LibraryExceptions
    {
        /// <summary>
        /// Property to retrieve the only instance of the Singleton.
        /// </summary>
        public static LibraryExceptions Instance
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
                            m_instance = new LibraryExceptions();
                        }
                    }
                }

                // Return the non-null instance of Singleton
                return m_instance;
            }
        }

        private void Set(Safir.Dob.Typesystem.FundamentalException exception)
        {
            Set(exception.GetTypeId(), exception.Message);
        }

        private void Set(Safir.Dob.Typesystem.Exception exception)
        {
            Set(exception.GetTypeId(), exception.Message);
        }

        /// <summary>
        /// Set the current exception.
        /// <para>
        /// When you have caught an exception that you would like to pass across language
        /// boundaries, call this method with the exception. Then, on the other side of
        /// the language boundary, call the Throw method, which will throw the exception in the other language.
        /// </para>
        /// </summary>
        /// <param name="exception">The exception.</param>
        public void Set(System.Exception exception)
        {
            Safir.Dob.Typesystem.Exception exc = exception as Safir.Dob.Typesystem.Exception;
            if (exc != null)
            {
                Set(exc);
                return;
            }

            Safir.Dob.Typesystem.FundamentalException fundExc = exception as Safir.Dob.Typesystem.FundamentalException;
            if (fundExc != null)
            {
                Set(fundExc);
                return;
            }
            Set(0,
                "Unknown (non-Dob) exception with name "
                + exception.GetType().FullName
                + "\n"
                + exception.Message);
        }

        private void Set(System.Int64 exceptionId, string description)
        {
            System.IntPtr desc = Internal.InternalOperations.CStringOf(description);
            Internal.Kernel.DotsC_SetException(exceptionId, desc);
            Marshal.FreeHGlobal(desc);
        }

        /// <summary>
        /// Throw the current exception.
        /// <para>
        /// Call this to throw the current exception. It is considered a programming
        /// error to call this function if no exception is set.
        /// </para>
        /// </summary>
        public void Throw()
        {
            byte wasSetByte;
            System.Int64 exceptionId;
            System.IntPtr description;
            Internal.Kernel.DotsC_BytePointerDeleter deleter;
            Internal.Kernel.DotsC_GetAndClearException(out exceptionId, out description, out deleter, out wasSetByte);
            bool wasSet = Internal.InternalOperations.BoolOf(wasSetByte);
            if (wasSet)
            {
                string desc = Internal.InternalOperations.StringOf(description);
                deleter(ref description);
                if (exceptionId == 0)
                {
                    throw new System.Exception(desc);
                }
                else
                {
                    System.Type type = GeneratedAssemblies.Instance.GetType(exceptionId);
                    System.Exception exc = System.Activator.CreateInstance(type, desc) as System.Exception;
                    if (exc == null)
                    {
                        throw new IllegalValueException("Could not create exception " + type.FullName);
                    }
                    throw exc;
                }
            }
            else
            {
                throw new SoftwareViolationException("There was no exception set when LibraryExceptions::Throw was called!");
            }
        }

        /// <summary>
        /// Check if the exception that will be thrown by Throw is of a certain type.
        /// </summary>
        /// <param name="exceptionTypeId">The exception type to compare to.</param>
        /// <returns>True if the given exception type will be thrown.</returns>
        public bool Peek(System.Int64 exceptionTypeId)
        {
            System.Int64 exceptionId;
            Internal.Kernel.DotsC_PeekAtException(out exceptionId);
            return Internal.InternalOperations.BoolOf(Internal.Kernel.DotsC_IsOfType(exceptionId, exceptionTypeId));
        }

        // Static, VOLATILE variable to store single instance
        private static volatile LibraryExceptions m_instance;

        // Static synchronization root object, for locking
        private static object m_instantiationLock = new object();

    }
}
