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
 * For internal usage. Use only if you know what you are doing.
 */
public class InternalOperations
{
    /**
     * format a piece of blank memory to be a blob of a desired type
     * does not check that the size of the blob is correct.
     * @param blob Blob to format.
     * @param blobSize Size of blob to format.
     * @param typeId Type id.
     * @return Pointer to unused part.
     */
    public static int formatBlob(java.nio.ByteBuffer blob,
                                 int blobSize,
                                 long typeId)
    {
        if (! blob.isDirect()){
            throw new SoftwareViolationException("blob ByteBuffer must be a 'direct' java.nio.ByteBuffer");
        }
        int beginningOfUnused[] = new int [1];
        Kernel.FormatBlob(blob, blobSize, typeId, beginningOfUnused);
        return beginningOfUnused[0];
    }
}
