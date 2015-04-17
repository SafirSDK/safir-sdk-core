/******************************************************************************
*
* Copyright Saab AB, 2007-2013 (http://safir.sourceforge.net)
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

#ifndef __DOSE_DLL_CALLBACKS_H__
#define __DOSE_DLL_CALLBACKS_H__

#include <Safir/Dob/Typesystem/EntityId.h>
#include <Safir/Dob/Typesystem/HandlerId.h>
#include <Safir/Dob/Defs.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    class Callbacks
    {
    public:
        static void OnDispatch(void* const consumer,
                               bool & success);

        static void OnStopOrder(void* const consumer,
                                bool & success);

        static void OnNewEntity(const char* const currentBlob,
                                const char* const currentState,
                                void* const consumer,
                                const bool timestampDiff,
                                bool& success);

        static void OnUpdatedEntity(const char* const currentBlob,
                                    const char* const currentState,
                                    const char* const previousBlob,
                                    const char* const previousState,
                                    void* const consumer,
                                    const bool timestampDiff,
                                    bool& success);

        static void OnDeletedEntity(const char* const currentState,
                                    const char* const previousBlob,
                                    const char* const previousState,
                                    const bool explicitlyDeleted,
                                    void* const consumer,
                                    const bool timestampDiff,
                                    bool& success);

        static void OnCreateRequest(const char* const requestBlob,
                                    const char* const state,
                                    const DotsC_Int32 ctrl,
                                    const Safir::Dob::Typesystem::Int32 responseId,
                                    void* const consumer,
                                    bool& success);

        static void OnUpdateRequest(const char* const requestBlob,
                                    const char* const state,
                                    const DotsC_Int32 ctrl,
                                    const Safir::Dob::Typesystem::Int32 responseId,
                                    void* const consumer,
                                    bool& success);

        static void OnDeleteRequest(const char* const state,
                                    const DotsC_Int32 ctrl,
                                    const Safir::Dob::Typesystem::Int32 responseId,
                                    void* const consumer,
                                    bool& success);

        static void OnServiceRequest(const char* const requestBlob,
                                     const char* const state,
                                     const DotsC_Int32 ctrl,
                                     const Safir::Dob::Typesystem::Int32 responseId,
                                     void* const consumer,
                                     bool& success);

        static void OnResponse(const Safir::Dob::RequestId requestId,
                               const char* const responseBlob,
                               const char* const responseState,
                               const char* const requestBlob,
                               const char* const requestState,
                               void* const consumer,
                               bool& success);

        static void OnMessage(const char * const message,
                              const char * const state,
                              void* const consumer,
                              bool & success);

        static void OnRegistered(const Safir::Dob::Typesystem::TypeId typeId,
                                 const Safir::Dob::Typesystem::Int64 handlerId,
                                 const char* const handlerIdStr,
                                 void* const consumer,
                                 bool& success);

        static void OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId,
                                   const Safir::Dob::Typesystem::Int64 handlerId,
                                   const char* const handlerIdStr,
                                   void* const consumer,
                                   bool& success);

        static void OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                          const Safir::Dob::Typesystem::Int64 handlerId,
                                          const char* const handlerIdStr,
                                          void* const consumer,
                                          bool& success);

        static void OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                            const Safir::Dob::Typesystem::Int64 handlerId,
                                            const char* const handlerIdStr,
                                            void* const consumer,
                                            bool& success);

        static void OnInjectedNewEntity(const char* const injectionBlob,
                                        const char* const injectionState,
                                        void* const consumer,
                                        bool& success);

        static void OnInjectedUpdatedEntity(const char* const injectionBlob,
                                            const char* const injectionState,
                                            const char* const currentBlob,
                                            const char* const currentState,
                                            void* const consumer,
                                            bool& success);

        static void OnInjectedDeletedEntity(const char* const injectionState,
                                            const char* const currentBlob,
                                            const char* const currentState,
                                            void* const consumer,
                                            bool& success);

        static void OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                            const Safir::Dob::Typesystem::Int64 handlerId,
                                            const char* const handlerIdStr,
                                            void* const consumer,
                                            bool& success);

        static void OnNotRequestOverflow(void* const consumer,
                                         bool & success);

        static void OnNotMessageOverflow(void* const consumer,
                                         bool & success);

        static void OnDropReference(void* const consumer,
                                    bool & success);
    };
}
}
}

#endif

