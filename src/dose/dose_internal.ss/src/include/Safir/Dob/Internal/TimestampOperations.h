/******************************************************************************
*
* Copyright Saab AB, 2008-2013 (http://safir.sourceforge.net)
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
#ifndef __TIMESTAMPOPERATIONS_H__
#define __TIMESTAMPOPERATIONS_H__


#include <Safir/Dob/Internal/InternalExportDefs.h>
#include <Safir/Dob/Internal/InternalFwd.h>
#include <Safir/Dob/Typesystem/Object.h>
namespace Safir
{
namespace Dob
{
namespace Internal
{
    class DOSE_INTERNAL_API TimestampOperations
    {
    public:

        typedef std::pair<DistributionData, bool> MergeResult;

        /** Merge the members from "from" into the object "into" and return the result.
         * The parameters are left unchanged.
         */
        static const MergeResult Merge(const DistributionData& into,
                                       const DistributionData& from);


        /** Merge changed members in blob into "into" using timestamp as the timestamp for changed members.
         * This function works ok if blob is null. The result will be a state without blob if the timestamp
         * is later than timestamps in "into".
         */
        static const MergeResult Merge(const DistributionData& into,
                                       const char* const blob,
                                       const Typesystem::Int64 timestamp);

        /** Set the timestamp on members that have their change flag set.
         * The top level timestamp is also set.
         * If no members have their changeflags set only the top timestamp will be set.
         *
         * resetUnchangedMembers: True => The timestamp for members that don't have their change flag set will be set to 0.
         *                        False => The timestamp for members that don't have their change flag set will not be touched.
         */
        static void SetTimestampForChangedMembers(DistributionData&        entityState,
                                                  const Typesystem::Int64  timestamp,
                                                  const bool               resetUnchangedMembers);

        /** Set the timestamp for all members
         * The top level timestamp is also set.
         */
        static void SetTimestampForAllMembers(DistributionData&        entityState,
                                              const Typesystem::Int64  timestamp);

        /** Set change flags in "inBlob" for all members that are newer in "previous" than in "current" */
        static void SetChangeFlags(const DistributionData& previous,
                                   const DistributionData& current,
                                   char* const inBlob);

        /** Returns true if realState contains all the data in injection. */
        static bool HaveChanges(const DistributionData & realState, const DistributionData & injection);

    private:

        typedef std::pair<Typesystem::Int64, bool> MergeMemberResult;

        static MergeMemberResult MergeMember(const Typesystem::TypeId typeId,
                                             const Typesystem::MemberIndex member,
                                             const Typesystem::Int64 intoTimestamp,
                                             const Typesystem::Int64 fromTimestamp,
                                             const Typesystem::ObjectPtr& intoObject,
                                             const Typesystem::ObjectConstPtr& fromObject);
        //Not instantiable
        TimestampOperations() = delete;
        ~TimestampOperations() = delete;
    };
}
}
}

#endif

