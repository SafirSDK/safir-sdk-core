/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safirsdkcore.com)
* 
* Created by: Stefan Lindström / stsyli
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

using System.Xml.Serialization;
using Safir.Dob.Typesystem;

namespace Sate
{
    /* Base class for containing an Object */

    [XmlInclude(typeof(EntityInfo))]
    [XmlInclude(typeof(MessageInfo))]
    [XmlInclude(typeof(ServiceHandlerInfo))]
    public class ObjectInfo
    {
        [XmlIgnore]
        public Object Obj { get; set; }

        public int Blobsize { get; set; }
    }
}