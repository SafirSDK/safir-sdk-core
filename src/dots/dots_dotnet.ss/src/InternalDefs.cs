/* ****************************************************************************
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

using System;
using System.Runtime.InteropServices;

namespace Safir.Dob.Typesystem.Internal
{
    internal class Constants
    {
        public const string DEFAULT_CHANNEL_ID_STR = "DEFAULT_CHANNEL";
        public static readonly Int64 DEFAULT_CHANNEL_ID = Id.Generate64BitHash(DEFAULT_CHANNEL_ID_STR);

        public const string ALL_CHANNELS_ID_STR = "ALL_CHANNELS";
        public static readonly Int64 ALL_CHANNELS_ID = Id.Generate64BitHash(ALL_CHANNELS_ID_STR); 
        
        public const string DEFAULT_HANDLER_ID_STR = "DEFAULT_HANDLER";
        public static readonly Int64 DEFAULT_HANDLER_ID = Id.Generate64BitHash(DEFAULT_HANDLER_ID_STR);

        public const string ALL_HANDLERS_ID_STR = "ALL_HANDLERS";
        public static readonly Int64 ALL_HANDLERS_ID = Id.Generate64BitHash(ALL_HANDLERS_ID_STR);
    }

    [StructLayout(LayoutKind.Sequential, Pack = 1)]
    internal struct DotsC_EntityId
    {
        public System.Int64 TypeId;
        public System.Int64 InstanceId;
    };

    internal enum DotsC_PropertyMappingKind
    {
        MappedToNull,
        MappedToMember,
        MappedToParameter,
    }
}
