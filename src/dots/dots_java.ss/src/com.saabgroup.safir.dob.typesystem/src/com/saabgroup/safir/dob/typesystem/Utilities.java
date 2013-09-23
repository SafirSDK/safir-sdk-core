// -*- coding: utf-8 -*-
/******************************************************************************
*
* Copyright Saab AB, 2005-2013 (http://safir.sourceforge.net)
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

package com.saabgroup.safir.dob.typesystem;


/**
 * Utility functions for users of the DOB type system.
 */
public class Utilities {

    /**
     * Merge the changed members (recursively) from one object into another.
     *
     * This function will recurse through the members of the "from" object and
     * take all the members that have a change flag set and copy them into the "into"
     * object.
     *
     * @param into Object to merge into.
     * @param from Object whose changes shall be merged into "into".
     */
    public static void mergeChanges(com.saabgroup.safir.dob.typesystem.Object into,
                                    com.saabgroup.safir.dob.typesystem.Object from) {
        if (from == null || into == null)
        {
            throw new SoftwareViolationException("Objects must not be null in call to MergeChanges");
        }

        if (from.getTypeId() != into.getTypeId())
        {
            throw new SoftwareViolationException("Objects must have same TypeId for MergeChanges");
        }

        try
        {
            int numMembers = Members.getNumberOfMembers(into.getTypeId());
            for (int member = 0; member < numMembers; ++member)
            {
                int arraySize = Members.getArraySize(into.getTypeId(), member);
                for (int index = 0; index < arraySize; ++index)
                {
                    ContainerBase fromContainerB = from.getMember(member,index);
                    ContainerBase intoContainerB = into.getMember(member,index);

                    //is it an object member?
                    if (fromContainerB instanceof ObjectContainerBase)
                    {
                        ObjectContainerBase fromContainerOB = (ObjectContainerBase)fromContainerB;
                        ObjectContainerBase intoContainerOB = (ObjectContainerBase)intoContainerB;

                        if (fromContainerOB.isChangedHere()) //this specific member has changed
                        {
                            if (fromContainerOB.isNull())
                            {
                                intoContainerOB.setNull();
                            }
                            else
                            {
                                intoContainerOB.setObjInternal(fromContainerOB.getObjInternal().clone());
                                intoContainerOB.setChangedHere(true);
                            }
                        }
                        else if (fromContainerOB.isChanged()) //some child has changed we need to recurse
                        {
                            //unless the type has changed or the into-member is null
                            if (intoContainerOB.isNull() || intoContainerOB.getObjInternal().getTypeId() != fromContainerOB.getObjInternal().getTypeId())
                            {
                                //If the type is changing we write a warning
                                if (!intoContainerOB.isNull())
                                {
                                    System.out.println("Warning (Contact a DOB developer if you do not understand it):");
                                    System.out.println("The type of a member has changed without the change flag being set in 'from'.");
                                }

                                //if it was null we don't warn (even if it is a little bit suspicious to do that...)

                                intoContainerOB.setObjInternal(fromContainerOB.getObjInternal().clone());
                                intoContainerOB.setChangedHere(true);
                            }
                            else
                            {
                                //recurse
                                mergeChanges(intoContainerOB.getObjInternal(), fromContainerOB.getObjInternal());
                            }
                        }
                    }
                    else //no, normal member
                    {
                        if (fromContainerB.isChanged())
                        {
                            intoContainerB.copy(fromContainerB);
                        }
                    }
                }
            }
        }
        catch (ClassCastException exc)
        {
            throw new SoftwareViolationException("Cast failed inside MergeChanges" + exc);
        }

    }


    /**
     * Converts binary data to Base64.
     *
     * Will convert the binarySource to Base64 format.
     *
     * @param binary Binary data to convert.
     * @return A string containing the Base64 representation of the binary source.
     */
    public static String binaryToBase64(byte[] binary)
    {
        java.nio.ByteBuffer buf = java.nio.ByteBuffer.allocateDirect(binary.length);
        buf.put(binary);
        return Kernel.BinaryToBase64(buf,binary.length);
    }

    /**
     * Converts data in Base64 format into binary data format.
     *
     * Will convert data from Base64 format to binary format.
     *
     * @param base64 [in] - Base64 data to convert.
     * @return Data converted to binary format.
     */
    public static byte[] base64ToBinary(String base64)
    {
        return Kernel.Base64ToBinary(base64);
    }

}
