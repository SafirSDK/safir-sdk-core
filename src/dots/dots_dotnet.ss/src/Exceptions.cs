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

namespace Safir.Dob.Typesystem
{
    /// <summary>
    /// This is the base class of all Fundamental Exceptions.
    /// <para/>
    /// All exceptions that signify "static errors" should inherit from this class.
    /// <para/>
    /// Fundamental Exceptions are exceptions that are only expected to be caught
    /// by the main loop in applications. They usually mean that something has gone very
    /// wrong. 
    /// </summary>
    public abstract class FundamentalException : System.Exception
    {
        /// <summary>
        /// Constructor with exception information.
        /// <para/>
        /// Creates an exception that contains information about why the exception
        /// occurred.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public FundamentalException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public const System.Int64 ExceptionTypeId = 6297553511903368764;

        /// <summary>
        /// Get the type id. To be overidden by subclasses. 
        /// </summary>
        /// <returns>Type id.</returns>
        public virtual System.Int64 GetTypeId() { return ExceptionTypeId; }
    }

    /// <summary>
    /// This is the base class of all (non-Fundamental) Exceptions.
    /// In comparis to FundamentalException the exceptions that derive from Exception
    /// are more of a dynamic nature. These exceptions are expected to be handled directly
    /// by the calling function and are possible to recover from.
    /// </summary>
    public abstract class Exception : System.Exception
    {
        /// <summary>
        /// Constructor with exception information.
        /// <para/>
        /// Creates an exception that contains information about why the exception occurred.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public Exception(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public const System.Int64 ExceptionTypeId = -2177307456017217044;

        /// <summary>
        /// Get the type id. To be overidden by subclasses. 
        /// </summary>
        /// <returns>Type id.</returns>
        public virtual System.Int64 GetTypeId() {return ExceptionTypeId;}
    }

    /// <summary>
    /// A parameter in the call was invalid.
    /// <para/>
    /// One of the parameters in the call was out of range or unexpected in some other way.
    /// </summary>
    public class IllegalValueException : FundamentalException
    {
        /// <summary>
        /// Constructor with exception information.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public IllegalValueException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public new const System.Int64 ExceptionTypeId = -3653935143986901894;

        /// <summary>
        /// Get the type id.
        /// </summary>
        /// <returns>Type id.</returns>
        public override System.Int64 GetTypeId() {return ExceptionTypeId;}
    }

    /// <summary>
    /// This exception is thrown if a class cannot be cast to the expected type.
    /// This usually signals a programming error in the client code./// 
    /// </summary>
    public class IncompatibleTypesException : FundamentalException
    {
        /// <summary>
        /// Constructor with exception information.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public IncompatibleTypesException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public new const System.Int64 ExceptionTypeId = -5150658527844777416;

        /// <summary>
        /// Get the type id.
        /// </summary>
        /// <returns>Type id.</returns>
        public override System.Int64 GetTypeId() { return ExceptionTypeId; }
    }

    /// <summary>
    /// Used when there is an error that implies that there is something wrong in the configuration.
    /// </summary>
    public class ConfigurationErrorException : FundamentalException
    {
        /// <summary>
        /// Constructor with exception information.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public ConfigurationErrorException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public new const System.Int64 ExceptionTypeId = 2909620812590558895;

        /// <summary>
        /// Get the type id.
        /// </summary>
        /// <returns>Type id.</returns>
        public override System.Int64 GetTypeId() { return ExceptionTypeId; }
    }

    /// <summary>
    /// Meant to be used when something goes very wrong.
    /// Indicates that there is a programming error somewhere.
    /// </summary>
    public class SoftwareViolationException : FundamentalException
    {
        /// <summary>
        /// Constructor with exception information.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public SoftwareViolationException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public new const System.Int64 ExceptionTypeId = -2318636033853590373;

        /// <summary>
        /// Get the type id.
        /// </summary>
        /// <returns>Type id.</returns>
        public override System.Int64 GetTypeId() { return ExceptionTypeId; }
    }

    /// <summary>
    /// Thrown when an application attempts to get the value of a member that is null.
    /// </summary>
    public class NullException : FundamentalException
    {
        /// <summary>
        /// Constructor with exception information.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public NullException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public new const System.Int64 ExceptionTypeId = -6392953138294149211;

        /// <summary>
        /// Get the type id.
        /// </summary>
        /// <returns>Type id.</returns>
        public override System.Int64 GetTypeId() { return ExceptionTypeId; }
    }

    /// <summary>
    /// Used when someone tries to set a property that is mapped
    /// to something that cannot be changed.
    /// </summary>
    public class ReadOnlyException : FundamentalException
    {
        /// <summary>
        /// Constructor with exception information.
        /// </summary>
        /// <param name="message">A message describing why the exception occurred.</param>
        public ReadOnlyException(string message) : base(message)
        {
        }

        /// <summary>
        /// The TypeId of this exception.
        /// </summary>
        public new const System.Int64 ExceptionTypeId = -4804695341042352897;

        /// <summary>
        /// Get the type id.
        /// </summary>
        /// <returns>Type id.</returns>
        public override System.Int64 GetTypeId() { return ExceptionTypeId; }
    }

}
