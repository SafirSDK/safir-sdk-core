/* ****************************************************************************
*
* Copyright Consoden AB, 2005-2015 (http://safir.sourceforge.net)
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
/// Utility functions for users of the DOB type system.
/// </summary>
public class Utilities
{
    /// <summary>
    /// Merge the changed members (recursively) from one object into another.
    ///
    /// <para/>
    /// This function will recurse through the members of the "from" object and
    /// take all the members that have a change flag set and copy them into the "into"
    /// object.
    /// </summary>
    /// <remarks>
    ///   <para>
    ///  Note that this uses shallow copying, so the two objects may end up
    ///  using the same objects internally.
    ///   </para>
    /// </remarks>
    /// <param name="into">Object to merge into.</param>
    /// <param name="from">Object whose changes shall be merged into "into".</param>
    public static void MergeChanges(Object into, Object from)
    {
        if (from == null || into == null)
        {
            throw new SoftwareViolationException("Objects must not be null in call to MergeChanges");
        }

        if (from.GetTypeId() != into.GetTypeId())
        {
            throw new SoftwareViolationException("Objects must have same TypeId for MergeChanges");
        }

        try
        {
            System.Int32 numMembers = Members.GetNumberOfMembers(into.GetTypeId());
            for (System.Int32 member = 0; member < numMembers; ++member)
            {
                System.Int32 arraySize = Members.GetArraySize(into.GetTypeId(), member);
                for (System.Int32 index = 0; index < arraySize; ++index)
                {
                    ContainerBase fromContainerB = from.GetMember(member,index);
                    ContainerBase intoContainerB = into.GetMember(member,index);

                    //is it an object member?
                    if (fromContainerB is ObjectContainerBase)
                    {
                        ObjectContainerBase fromContainerOB = fromContainerB as ObjectContainerBase;
                        ObjectContainerBase intoContainerOB = intoContainerB as ObjectContainerBase;

                        if (fromContainerOB.IsChangedHere()) //this specific member has changed
                        {
                            intoContainerOB.ShallowCopy(fromContainerOB);
                        }
                        else if (fromContainerOB.IsChanged()) //some child has changed we need to recurse
                        {
                            //unless the type has changed or the into-member is null
                            if (intoContainerOB.IsNull() || intoContainerOB.InternalObj.GetTypeId() != fromContainerOB.InternalObj.GetTypeId())
                            {
                                //If the type is changing we write a warning
                                if (!intoContainerOB.IsNull())
                                {
                                    Safir.Logging.SendSystemLog(Safir.Logging.Severity.Warning,
                                                                "The type of a member has changed without the change flag being set in 'from'.");
                                }

                                //if it was null we don't warn (even if it is a little bit suspicious to do that...)

                                intoContainerOB.ShallowCopy(fromContainerOB);
                            }
                            else
                            {
                                //recurse
                                MergeChanges(intoContainerOB.InternalObj, fromContainerOB.InternalObj);
                            }
                        }
                    }
                    else //no, normal member
                    {
                        if (fromContainerB.IsChanged())
                        {
                            intoContainerB.ShallowCopy(fromContainerB);
                        }
                    }
                }
            }
        }
        catch (System.InvalidCastException exc)
        {
            throw new SoftwareViolationException("Cast failed inside MergeChanges" + exc.Message);
        }
    }
}
}
