/******************************************************************************
*
* Copyright Saab AB, 2007-2008 (http://www.safirsdk.com)
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
#include <Safir/Dob/Internal/ConnectionId.h>

namespace Safir
{
namespace Dob
{
namespace Internal
{
    //const long DOSE_LANGUAGE_CPP = 0; //defines C++ to dose_dll

    class Callbacks
    {
    public:
        static void __cdecl OnDispatch(void* const consumer,
                                       bool & success);

        static void __cdecl OnStopOrder(void* const consumer,
                                        bool & success);

        static void __cdecl OnNewEntity(const char* const currentBlob,
                                        const char* const currentState,
                                        void* const consumer,
                                        const bool timestampDiff,
                                        bool& success);

        static void __cdecl OnUpdatedEntity(const char* const currentBlob,
                                            const char* const currentState,
                                            const char* const previousBlob,
                                            const char* const previousState,
                                            void* const consumer,
                                            const bool timestampDiff,
                                            bool& success);

        static void __cdecl OnDeletedEntity(const char* const currentState,
                                            const char* const previousBlob,
                                            const char* const previousState,
                                            const bool explicitlyDeleted,
                                            void* const consumer,
                                            const bool timestampDiff,
                                            bool& success);

        static void __cdecl OnCreateRequest(const char* const requestBlob,
                                            const char* const state,
                                            const long ctrl,
                                            const Safir::Dob::Typesystem::Int32 responseId,
                                            void* const consumer,
                                            bool& success);

        static void __cdecl OnUpdateRequest(const char* const requestBlob,
                                            const char* const state,
                                            const long ctrl,
                                            const Safir::Dob::Typesystem::Int32 responseId,
                                            void* const consumer,
                                            bool& success);

        static void __cdecl OnDeleteRequest(const char* const state,
                                            const long ctrl,
                                            const Safir::Dob::Typesystem::Int32 responseId,
                                            void* const consumer,
                                            bool& success);

        static void __cdecl OnServiceRequest(const char* const requestBlob,
                                             const char* const state,
                                             const long ctrl,
                                             const Safir::Dob::Typesystem::Int32 responseId,
                                             void* const consumer,
                                             bool& success);

        static void __cdecl OnResponse(const Safir::Dob::RequestId requestId,
                                       const char* const responseBlob,
                                       const char* const responseState,
                                       const char* const requestBlob,
                                       const char* const requestState,
                                       void* const consumer,
                                       bool& success);

        static void __cdecl OnMessage(const char * const message,
                                      const char * const state,
                                      void* const consumer,
                                      bool & success);

        static void __cdecl OnRegistered(const Safir::Dob::Typesystem::TypeId typeId,
                                         const Safir::Dob::Typesystem::Int64 handlerId,
                                         const char* const handlerIdStr,
                                         void* const consumer,
                                         bool& success);

        static void __cdecl OnUnregistered(const Safir::Dob::Typesystem::TypeId typeId,
                                           const Safir::Dob::Typesystem::Int64 handlerId,
                                           const char* const handlerIdStr,
                                           void* const consumer,
                                           bool& success);

        static void __cdecl OnRevokedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                                  const Safir::Dob::Typesystem::Int64 handlerId,
                                                  const char* const handlerIdStr,
                                                  void* const consumer,
                                                  bool& success);

        static void __cdecl OnCompletedRegistration(const Safir::Dob::Typesystem::TypeId typeId,
                                                    const Safir::Dob::Typesystem::Int64 handlerId,
                                                    const char* const handlerIdStr,
                                                    void* const consumer,
                                                    bool& success);

        static void __cdecl OnInjectedNewEntity(const char* const injectionBlob,
                                                const char* const injectionState,
                                                void* const consumer,
                                                bool& success);

        static void __cdecl OnInjectedUpdatedEntity(const char* const injectionBlob,
                                                    const char* const injectionState,
                                                    const char* const currentBlob,
                                                    const char* const currentState,
                                                    void* const consumer,
                                                    bool& success);

        static void __cdecl OnInjectedDeletedEntity(const char* const injectionState,
                                                    const char* const currentBlob,
                                                    const char* const currentState,
                                                    void* const consumer,
                                                    bool& success);

        static void __cdecl OnInitialInjectionsDone(const Safir::Dob::Typesystem::TypeId typeId,
                                                    const Safir::Dob::Typesystem::Int64 handlerId,
                                                    const char* const handlerIdStr,
                                                    void* const consumer,
                                                    bool& success);

        static void __cdecl OnNotRequestOverflow(void* const consumer,
                                                 bool & success);

        static void __cdecl OnNotMessageOverflow(void* const consumer,
                                                 bool & success);

        static void __cdecl OnDropReference(void* const consumer,
                                            bool & success);
    };
}
}
}

#endif

