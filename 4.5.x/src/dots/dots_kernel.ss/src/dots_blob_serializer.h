/******************************************************************************
*
* Copyright Saab AB, 2004-2013 (http://safir.sourceforge.net)
* 
* Created by: Joel Ottosson / stjoot
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

#ifndef _dots_blob_serializer_h
#define _dots_blob_serializer_h

#include "dots_internal_defs.h"

namespace Safir
{
namespace Dob
{
namespace Typesystem
{
namespace Internal
{
    /**
     * Functionality for serialization of objects in xml format into binary format.
     */
    class XmlToBlobSerializer
    {
    public:
        XmlToBlobSerializer();
        ~XmlToBlobSerializer();

        //if successful the blob is returned
        //you are responsible for deleting the blob
        //Returns NULL if failure
        char * Serialize(const char* xmlSource);

        //help methods
        static bool ToTypeId(const std::string & str, TypeId & tid);
        static bool ToEntityId(const std::string & str, DotsC_EntityId & entityId, std::string & instanceIdStr);

    };


}
}
}
}
#endif
