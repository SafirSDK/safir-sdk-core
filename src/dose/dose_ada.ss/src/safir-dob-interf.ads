-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
--
--  Created by: Anders Widén / stawi
--
-------------------------------------------------------------------------------
--
--  This file is part of Safir SDK Core.
--
--  Safir SDK Core is free software: you can redistribute it and/or modify
--  it under the terms of version 3 of the GNU General Public License as
--  published by the Free Software Foundation.
--
--  Safir SDK Core is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
--
-------------------------------------------------------------------------------
with Safir.Dob.Defs;
with Interfaces.C;
with Interfaces.C.Strings;
with Safir.Dob.Callbacks;
with Safir.Dob.Typesystem;
with Safir.Dob.Consumer_Bases;
with Safir.Dob.Blob_References;

package Safir.Dob.Interf is

   package C renames Interfaces.C;

   Language_Ada : constant Safir.Dob.Typesystem.Int_32 := 1;

   --  //--------------------------------------------------------------------
   --  // Constants used to define from which language a call comes from
   --  //
   --  // All in-parameters called 'lang' is used by dose_dll to distinguish
   --  // which set of callbacks to use for a specific consumer. Make sure
   --  // you send the correct value of lang when using this API. See list
   --  // below.
   --  //--------------------------------------------------------------------
   --  const long DOSE_LANGUAGE_CPP    = 0;
   --  const long DOSE_LANGUAGE_ADA    = 1;
   --  const long DOSE_LANGUAGE_DOTNET = 2;
   --  const long DOSE_LANGUAGE_JAVA   = 3;
   --
   --  // Define which languages use garbage collection
   --  static const bool g_garbageCollected[] = {false, //C++
   --                                            false, //Ada
   --                                            true,  //Dotnet
   --                                            true}; //Java
   --  //--------------------------------------------------------------------
   --
   --  #ifdef __cplusplus
   --  extern "C"
   --  {
   --  #endif
   --
   --
   --      //---------------------------------------------------------------------------
   --      // Callback functions
   --      //---------------------------------------------------------------------------
   --      typedef void CALLING_CONVENTION OnDispatchCb(void* const consumer,
   --                                                   bool& success);
   --
   --      typedef void CALLING_CONVENTION OnStopOrderCb(void* const consumer,
   --                                                    bool& success);
   --
   --      typedef void CALLING_CONVENTION OnNewEntityCb(const char* const currentBlob,
   --                                                    const char* const currentState,
   --                                                    void* const consumer,
   --                                                    const bool timestampDiff,
   --                                                    bool& success);
   --
   --      typedef void CALLING_CONVENTION OnUpdatedEntityCb(const char* const currentBlob,
   --                                                        const char* const currentState,
   --                                                        const char* const previousBlob,
   --                                                        const char* const previousState,
   --                                                        void* const consumer,
   --                                                        const bool timestampDiff,
   --                                                        bool& success);
   --
   --      typedef void CALLING_CONVENTION OnDeletedEntityCb(const char* const currentState,
   --                                                        const char* const previousBlob,
   --                                                        const char* const previousState,
   --                                                        const bool explicitlyDeleted,
   --                                                        void* const consumer,
   --                                                        const bool timestampDiff,
   --                                                        bool& success);
   --
   --      typedef void CALLING_CONVENTION OnCreateRequestCb(const char* const requestBlob,
   --                                                        const char* const state,
   --                                                        const long ctrl,
   --                                                        const DotsC_Int32 responseId,
   --                                                        void* const consumer,
   --                                                        bool& success);
   --
   --      typedef void CALLING_CONVENTION OnUpdateRequestCb(const char* const requestBlob,
   --                                                        const char* const state,
   --                                                        const long ctrl,
   --                                                        const DotsC_Int32 responseId,
   --                                                        void* const consumer,
   --                                                        bool& success);
   --
   --      typedef void CALLING_CONVENTION OnDeleteRequestCb(const char* const state,
   --                                                        const long ctrl,
   --                                                        const DotsC_Int32 responseId,
   --                                                        void* const consumer,
   --                                                        bool& success);
   --
   --      typedef void CALLING_CONVENTION OnServiceRequestCb(const char* const requestBlob,
   --                                                         const char* const state,
   --                                                         const long ctrl,
   --                                                         const DotsC_Int32 responseId,
   --                                                         void* const consumer,
   --                                                         bool& success);
   --
   --      typedef void CALLING_CONVENTION OnResponseCb(const DoseC_RequestId requestId,
   --                                                   const char* const responseBlob,
   --                                                   const char* const responseState,
   --                                                   const char* const requestBlob,
   --                                                   const char* const requestState,
   --                                                   void* const consumer,
   --                                                   bool& success);
   --
   --      typedef void CALLING_CONVENTION OnMessageCb(const char* const message,
   --                                                  const char* const state,
   --                                                  void* const consumer,
   --                                                  bool& success);
   --
   --      typedef void CALLING_CONVENTION OnRegisteredCb(const DotsC_TypeId typeId,
   --                                                     const DotsC_Int64 handlerId,
   --                                                     const char* const handlerIdStr,
   --                                                     void* const consumer,
   --                                                     bool& success);
   --
   --      typedef void CALLING_CONVENTION OnUnregisteredCb(const DotsC_TypeId typeId,
   --                                                       const DotsC_Int64 handlerId,
   --                                                       const char* const handlerIdStr,
   --                                                       void* const consumer,
   --                                                       bool& success);
   --
   --      typedef void CALLING_CONVENTION OnRevokedRegistrationCb(const DotsC_TypeId typeId,
   --                                                              const DotsC_Int64 handlerId,
   --                                                              const char* const handlerIdStr,
   --                                                              void* const consumer,
   --                                                              bool& success);
   --
   --      typedef void CALLING_CONVENTION OnCompletedRegistrationCb(const DotsC_TypeId typeId,
   --                                                                const DotsC_Int64 handlerId,
   --                                                                const char* const handlerIdStr,
   --                                                                void* const consumer,
   --                                                                bool& success);
   --
   --      typedef void CALLING_CONVENTION OnInjectedNewEntityCb(const char* const injectionBlob,
   --                                                            const char* const injectionState,
   --                                                            void* const consumer,
   --                                                            bool& success);
   --
   --      typedef void CALLING_CONVENTION OnInjectedUpdatedEntityCb(const char* const injectionBlob,
   --                                                                const char* const injectionState,
   --                                                                const char* const currentBlob,
   --                                                                const char* const currentState,
   --                                                                void* const consumer,
   --                                                                bool& success);
   --
   --      typedef void CALLING_CONVENTION OnInjectedDeletedEntityCb(const char* const injectionState,
   --                                                                const char* const currentBlob,
   --                                                                const char* const currentState,
   --                                                                void* const consumer,
   --                                                                bool& success);
   --
   --      typedef void CALLING_CONVENTION OnInitialInjectionsDoneCb(const DotsC_TypeId typeId,
   --                                                                const DotsC_Int64 handlerId,
   --                                                                const char* const handlerIdStr,
   --                                                                void* const consumer,
   --                                                                bool& success);
   --
   --      typedef void CALLING_CONVENTION OnNotRequestOverflowCb(void* const consumer,
   --                                                             bool& success);
   --
   --      typedef void CALLING_CONVENTION OnNotMessageOverflowCb(void* const consumer,
   --                                                             bool& success);
   --
   --      typedef void CALLING_CONVENTION OnDropReferenceCb(void* const consumer,
   --                                                        const long refCounter,
   --                                                        bool& success);
   --
   --
   -------------------------------------
   -- Startup and Initialization methods
   -------------------------------------
   procedure Constructor (Controller_Id : out Safir.Dob.Defs.Controller_Id;
                          Success       : out C.char);
   pragma Import (C, Constructor, "DoseC_Constructor");

   procedure Destructor
     (Controller_Id : in     Safir.Dob.Defs.Controller_Id);
   pragma Import (C, Destructor, "DoseC_Destructor");

   procedure Is_Connected
     (Controller_Id : in Safir.Dob.Defs.Controller_Id;
      Is_Connected  : out C.char;
      Success       : out C.char);
   pragma Import (C, Is_Connected, "DoseC_IsConnected");

   procedure Connect
     (Controller_Id              : in     Safir.Dob.Defs.Controller_Id;
      ConnectionNameCommonPart   : in     C.char_array;
      ConnectionNameInstancePart : in     C.char_array;
      Context                    : in     Safir.Dob.Typesystem.Int_32;
      Language                   : in     Safir.Dob.Typesystem.Int_32;
      Stop_Handler               : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Dispatcher                 : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      On_Dispatch                : in Safir.Dob.Callbacks.On_Do_Dispatch_Cb_Ptr;
      On_Stop_Order              : in Safir.Dob.Callbacks.On_Stop_Order_Cb_Ptr;
      On_New_Entity              : in Safir.Dob.Callbacks.On_New_Entity_Cb_Ptr;
      On_Updated_Entity          : in Safir.Dob.Callbacks.On_Updated_Entity_Cb_Ptr;
      On_Deleted_Entity          : in Safir.Dob.Callbacks.On_Deleted_Entity_Cb_Ptr;
      On_Create_Request          : in Safir.Dob.Callbacks.On_Create_Request_Cb_Ptr;
      On_Update_Request          : in Safir.Dob.Callbacks.On_Update_Request_Cb_Ptr;
      On_Delete_Request          : in Safir.Dob.Callbacks.On_Delete_Request_Cb_Ptr;
      On_Service_Request         : in Safir.Dob.Callbacks.On_Service_Request_Cb_Ptr;
      On_Response                : in Safir.Dob.Callbacks.On_Response_Cb_Ptr;
      On_Message                 : in Safir.Dob.Callbacks.On_Message_Cb_Ptr;
      On_Registered              : in Safir.Dob.Callbacks.On_Registered_Cb_Ptr;
      On_Unregistered            : in Safir.Dob.Callbacks.On_Unregistered_Cb_Ptr;
      On_Revoked_Registration    : in Safir.Dob.Callbacks.On_Revoked_Registration_Cb_Ptr;
      On_Completed_Registration  : in Safir.Dob.Callbacks.On_Completed_Registration_Cb_Ptr;
      On_Injected_New_Entity     : in Safir.Dob.Callbacks.On_Injected_New_Entity_Cb_Ptr;
      On_Injected_Updated_Entity : in Safir.Dob.Callbacks.On_Injected_Updated_Entity_Cb_Ptr;
      On_Injected_Deleted_Entity : in Safir.Dob.Callbacks.On_Injected_Deleted_Entity_Cb_Ptr;
      On_Initial_Injections_Done : in Safir.Dob.Callbacks.On_Initial_Injections_Done_Cb_Ptr;
      On_Not_Request_Overflow    : in Safir.Dob.Callbacks.On_Not_Request_Overflow_Cb_Ptr;
      On_Not_Message_Overflow    : in Safir.Dob.Callbacks.On_Not_Message_Overflow_Cb_Ptr;
      On_Drop_Reference          : access Integer; -- Dummy for Ada
      Success                    : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Connect, "DoseC_Connect");

   procedure Connect_Secondary
     (ConnectionNameCommonPart   : in     C.char_array;
      ConnectionNameInstancePart : in     C.char_array;
      Language                   : in     Safir.Dob.Typesystem.Int_32;
      On_New_Entity              : in Safir.Dob.Callbacks.On_New_Entity_Cb_Ptr;
      On_Updated_Entity          : in Safir.Dob.Callbacks.On_Updated_Entity_Cb_Ptr;
      On_Deleted_Entity          : in Safir.Dob.Callbacks.On_Deleted_Entity_Cb_Ptr;
      On_Create_Request          : in Safir.Dob.Callbacks.On_Create_Request_Cb_Ptr;
      On_Update_Request          : in Safir.Dob.Callbacks.On_Update_Request_Cb_Ptr;
      On_Delete_Request          : in Safir.Dob.Callbacks.On_Delete_Request_Cb_Ptr;
      On_Service_Request         : in Safir.Dob.Callbacks.On_Service_Request_Cb_Ptr;
      On_Response                : in Safir.Dob.Callbacks.On_Response_Cb_Ptr;
      On_Message                 : in Safir.Dob.Callbacks.On_Message_Cb_Ptr;
      On_Registered              : in Safir.Dob.Callbacks.On_Registered_Cb_Ptr;
      On_Unregistered            : in Safir.Dob.Callbacks.On_Unregistered_Cb_Ptr;
      On_Revoked_Registration    : in Safir.Dob.Callbacks.On_Revoked_Registration_Cb_Ptr;
      On_Completed_Registration  : in Safir.Dob.Callbacks.On_Completed_Registration_Cb_Ptr;
      On_Injected_New_Entity     : in Safir.Dob.Callbacks.On_Injected_New_Entity_Cb_Ptr;
      On_Injected_Updated_Entity : in Safir.Dob.Callbacks.On_Injected_Updated_Entity_Cb_Ptr;
      On_Injected_Deleted_Entity : in Safir.Dob.Callbacks.On_Injected_Deleted_Entity_Cb_Ptr;
      On_Initial_Injections_Done : in Safir.Dob.Callbacks.On_Initial_Injections_Done_Cb_Ptr;
      On_Not_Request_Overflow    : in Safir.Dob.Callbacks.On_Not_Request_Overflow_Cb_Ptr;
      On_Not_Message_Overflow    : in Safir.Dob.Callbacks.On_Not_Message_Overflow_Cb_Ptr;
      On_Drop_Reference          : access Integer; -- Dummy for Ada
      New_Ctrl_Id                : out Safir.Dob.Defs.Controller_Id;
      Success                    : out C.char);
   pragma Import (C, Connect_Secondary, "DoseC_ConnectSecondary");

   procedure Disconnect
     (Controller_Id : in  Safir.Dob.Defs.Controller_Id;
      Check_Thread  : in  C.char;
      Success       : out C.char);
   pragma Import (C, Disconnect, "DoseC_Disconnect");

   procedure Get_Connection_Name
     (Controller_Id : in  Safir.Dob.Defs.Controller_Id;
      Name          : out C.Strings.chars_ptr;
      Success       : out C.char);
   pragma Import (C, Get_Connection_Name, "DoseC_GetConnectionName");

   procedure Get_Connection_Name_Common_Part
     (Controller_Id : in  Safir.Dob.Defs.Controller_Id;
      Name          : out C.Strings.chars_ptr;
      Success       : out C.char);
   pragma Import (C, Get_Connection_Name_Common_Part, "DoseC_GetConnectionNameCommonPart");

   procedure Get_Connection_Name_Instance_Part
     (Controller_Id : in  Safir.Dob.Defs.Controller_Id;
      Name          : out C.Strings.chars_ptr;
      Success       : out C.char);
   pragma Import (C, Get_Connection_Name_Instance_Part, "DoseC_GetConnectionNameInstancePart");

   procedure Register_Service_Handler
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Override_Registration : in C.char;
      Language              : in Safir.Dob.Typesystem.Int_32;
      Service_Handler       : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success               : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Register_Service_Handler, "DoseC_RegisterServiceHandler");

   procedure Register_Entity_Handler
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Instance_Id_Policy    : in Safir.Dob.Typesystem.Enumeration_Value;
      Override_Registration : in C.char;
      Injection_Handler     : in C.char;
      Language              : in Safir.Dob.Typesystem.Int_32;
      Entity_Handler        : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success               : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Register_Entity_Handler, "DoseC_RegisterEntityHandler");

   procedure Unregister_Handler
     (Controller_Id      : in Safir.Dob.Defs.Controller_Id;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id         : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str     : in C.Strings.chars_ptr;
      Success            : out C.char);
   pragma Import (C, Unregister_Handler, "DoseC_UnregisterHandler");

   -----------------------
   -- Subscription methods
   -----------------------

   procedure Subscribe_Message
     (Controller_Id      : in Safir.Dob.Defs.Controller_Id;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Channel_Id         : in Safir.Dob.Typesystem.Int_64;
      Channel_Id_Str     : in C.Strings.chars_ptr;
      Include_Subclasses : in C.char;
      Language           : in Safir.Dob.Typesystem.Int_32;
      Message_Subscriber : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success            : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Subscribe_Message, "DoseC_SubscribeMessage");

   procedure Unsubscribe_Message
     (Controller_Id      : in Safir.Dob.Defs.Controller_Id;
      Type_Id            : in Safir.Dob.Typesystem.Type_Id;
      Channel_Id         : in Safir.Dob.Typesystem.Int_64;
      Channel_Id_Str     : in C.Strings.chars_ptr;
      Include_Subclasses : in C.char;
      Language           : in Safir.Dob.Typesystem.Int_32;
      Message_Subscriber : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success            : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Unsubscribe_Message, "DoseC_UnsubscribeMessage");

   procedure Subscribe_Entity
     (Controller_Id        : in Safir.Dob.Defs.Controller_Id;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id          : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str      : in C.Strings.chars_ptr;
      All_Instances        : in C.char;
      Include_Updates      : in C.char;
      Include_Subclasses   : in C.char;
      Restart_Subscription : in C.char;
      Language             : in Safir.Dob.Typesystem.Int_32;
      Entity_Subscriber    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success              : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Subscribe_Entity, "DoseC_SubscribeEntity");

   procedure Injector_Subscribe_Entity
     (Controller_Id                        : in Safir.Dob.Defs.Controller_Id;
      Type_Id                              : in Safir.Dob.Typesystem.Type_Id;
      Include_Updates                      : in C.char;
      Include_Subclasses                   : in C.char;
      Restart_Subscription                 : in C.char;
      Wants_Ghost_Delete                   : in C.char;
      Wants_Last_State                     : in C.char;
      Doesnt_Want_Source_Is_PermanentStore : in C.char;
      Wants_All_State_Changes              : in C.char;
      Timestamp_Change_Info                : in C.char;
      Language                             : in Safir.Dob.Typesystem.Int_32;
      Entity_Subscriber                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                              : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Injector_Subscribe_Entity, "DoseC_InjectorSubscribeEntity");

   procedure Unsubscribe_Entity
     (Controller_Id        : in Safir.Dob.Defs.Controller_Id;
      Type_Id              : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id          : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str      : in C.Strings.chars_ptr;
      All_Instances        : in C.char;
      Include_Subclasses   : in C.char;
      Language             : in Safir.Dob.Typesystem.Int_32;
      Entity_Subscriber    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success              : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Unsubscribe_Entity, "DoseC_UnsubscribeEntity");

   procedure Subscribe_Registration
     (Controller_Id           : in Safir.Dob.Defs.Controller_Id;
      Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id              : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str          : in C.Strings.chars_ptr;
      Include_Subclasses      : in C.char;
      Restart_Subscription    : in C.char;
      Language                : in Safir.Dob.Typesystem.Int_32;
      Registration_Subscriber : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success            : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Subscribe_Registration, "DoseC_SubscribeRegistration");

   procedure Unsubscribe_Registration
     (Controller_Id           : in Safir.Dob.Defs.Controller_Id;
      Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id              : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str          : in C.Strings.chars_ptr;
      Include_Subclasses      : in C.char;
      Language                : in Safir.Dob.Typesystem.Int_32;
      Registration_Subscriber : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success            : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Unsubscribe_Registration, "DoseC_UnsubscribeRegistration");

   procedure Dispatch (Controller_Id : in Safir.Dob.Defs.Controller_Id;
                       Success       : out C.char);
   pragma Import (C, Dispatch, "DoseC_Dispatch");

   procedure Exit_Dispatch (Controller_Id : in Safir.Dob.Defs.Controller_Id;
                            Success       : out C.char);
   pragma Import (C, Exit_Dispatch, "DoseC_ExitDispatch");

   procedure Get_Current_Callback_Id (Controller_Id : in Safir.Dob.Defs.Controller_Id;
                                      Callback_Id   : out Safir.Dob.Typesystem.Int_32;
                                      Success       : out C.char);
   pragma Import (C, Get_Current_Callback_Id, "DoseC_GetCurrentCallbackId");

   -----------------
   -- Message method
   -----------------

   procedure Send_Message
     (Controller_Id  : in Safir.Dob.Defs.Controller_Id;
      Message        : in Safir.Dob.Typesystem.Blob_T;
      Channel_Id     : in Safir.Dob.Typesystem.Int_64;
      Channel_Id_Str : in C.Strings.chars_ptr;
      Language       : in Safir.Dob.Typesystem.Int_32;
      Message_Sender : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success        : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Send_Message, "DoseC_SendMessage");

   ------------------------------
   -- Request methods
   ------------------------------
   procedure Service_Request
     (Controller_Id  : in Safir.Dob.Defs.Controller_Id;
      Request        : in Safir.Dob.Typesystem.Blob_T;
      Handler_Id     : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str : in C.Strings.chars_ptr;
      Language       : in Safir.Dob.Typesystem.Int_32;
      Requestor      : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Request_Id     : out Safir.Dob.Defs.Request_Id;
      Success        : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Service_Request, "DoseC_ServiceRequest");

   procedure Create_Request
     (Controller_Id   : in Safir.Dob.Defs.Controller_Id;
      Request         : in Safir.Dob.Typesystem.Blob_T;
      Has_Instance_Id : in C.char;
      Instance_Id     : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str : in C.Strings.chars_ptr;
      Handler_Id      : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str  : in C.Strings.chars_ptr;
      Language        : in Safir.Dob.Typesystem.Int_32;
      Requestor       : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Request_Id      : out Safir.Dob.Defs.Request_Id;
      Success         : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Create_Request, "DoseC_CreateRequest");

   procedure Update_Request
     (Controller_Id   : in Safir.Dob.Defs.Controller_Id;
      Request         : in Safir.Dob.Typesystem.Blob_T;
      Instance_Id     : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str : in C.Strings.chars_ptr;
      Language        : in Safir.Dob.Typesystem.Int_32;
      Requestor       : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Request_Id      : out Safir.Dob.Defs.Request_Id;
      Success         : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Update_Request, "DoseC_UpdateRequest");

   procedure Delete_Request
     (Controller_Id   : in Safir.Dob.Defs.Controller_Id;
      Type_Id         : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id     : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str : in C.Strings.chars_ptr;
      Language        : in Safir.Dob.Typesystem.Int_32;
      Requestor       : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Request_Id      : out Safir.Dob.Defs.Request_Id;
      Success         : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Delete_Request, "DoseC_DeleteRequest");

   procedure Send_Response
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Response              : in Safir.Dob.Typesystem.Blob_T;
      Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Language              : in Safir.Dob.Typesystem.Int_32;
      Response_Id           : in Safir.Dob.Defs.Response_Id;
      Success               : out C.char);
   -- CPP instead of C to get rid of warnings that we points to tagged types
   pragma Import (CPP, Send_Response, "DoseC_SendResponse");

   procedure Set_Entity
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Entity                : in Safir.Dob.Typesystem.Blob_T;
      Instance_Id           : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str       : in C.Strings.chars_ptr;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Consider_Change_Flags : in C.char;
      Initial_Injection     : in C.char;
      Success               : out C.char);
   pragma Import (C, Set_Entity, "DoseC_SetEntity");

   procedure Delete_Entity
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id           : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str       : in C.Strings.chars_ptr;
      All_Instances         : in C.char;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Success               : out C.char);
   pragma Import (C, Delete_Entity, "DoseC_DeleteEntity");

   procedure Inject_Entity
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Entity                : in Safir.Dob.Typesystem.Blob_T;
      Instance_Id           : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str       : in C.Strings.chars_ptr;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Timestamp             : in Safir.Dob.Typesystem.Int_64;
      Success               : out C.char);
   pragma Import (C, Inject_Entity, "DoseC_InjectEntity");

   procedure Inject_Deleted_Entity
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id           : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str       : in C.Strings.chars_ptr;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Timestamp             : in Safir.Dob.Typesystem.Int_64;
      Success               : out C.char);
   pragma Import (C, Inject_Deleted_Entity, "DoseC_InjectDeletedEntity");

   procedure Read_Entity
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id           : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str       : in C.Strings.chars_ptr;
      Current_Blob          : out Safir.Dob.Typesystem.Blob_T;
      Current_State         : out Safir.Dob.Typesystem.Char_Star;
      Success               : out C.char);
   pragma Import (C, Read_Entity, "DoseC_ReadEntity");

   procedure Is_Created
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Instance_Id           : in Safir.Dob.Typesystem.Int_64;
      Instance_Id_Str       : in C.Strings.chars_ptr;
      Is_Created            : out C.char;
      Success               : out C.char);
   pragma Import (C, Is_Created, "DoseC_IsCreated");

   procedure Get_Number_Of_Instances
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str        : in C.Strings.chars_ptr;
      Include_Subsclasses   : in C.char;
      Number_Of_Instances   : out Safir.Dob.Typesystem.Int_64;
      Success               : out C.char);
   pragma Import (C, Get_Number_Of_Instances, "DoseC_GetNumberOfInstances");

   procedure Get_Instance_Id_Policy
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Type_Id               : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id            : in Safir.Dob.Typesystem.Int_64;
      InstanceIdPolicy      : out Safir.Dob.Typesystem.Enumeration_Value;
      Success               : out C.char);
   pragma Import (C, Get_Instance_Id_Policy, "DoseC_GetInstanceIdPolicy");

   procedure Postpone
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Redispatch_Current    : in C.char;
      Success               : out C.char);
   pragma Import (C, Postpone, "DoseC_Postpone");

   procedure Resume_Postponed
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Success               : out C.char);
   pragma Import (C, Resume_Postponed, "DoseC_ResumePostponed");

   procedure Incomplete_Injection_State
     (Controller_Id         : in Safir.Dob.Defs.Controller_Id;
      Success               : out C.char);
   pragma Import (C, Incomplete_Injection_State, "DoseC_IncompleteInjectionState");

   procedure Get_Channel_Id
     (State                 : in Safir.Dob.Typesystem.Char_Star;
      Channel_Id            : out Safir.Dob.Typesystem.Int_64;
      Success               : out C.char);
   pragma Import (C, Get_Channel_Id, "DoseC_GetChannelId");

   procedure Get_Type_Id
     (State                 : in Safir.Dob.Typesystem.Char_Star;
      Type_Id               : out Safir.Dob.Typesystem.Type_Id;
      Success               : out C.char);
   pragma Import (C, Get_Type_Id, "DoseC_GetTypeId");

   procedure Get_Instance_Id
     (Current_State         : in Safir.Dob.Typesystem.Char_Star;
      Instance_Id           : out Safir.Dob.Typesystem.Int_64;
      Success               : out C.char);
   pragma Import (C, Get_Instance_Id, "DoseC_GetInstanceId");

   procedure Get_Handler_Id
     (Current_State         : in Safir.Dob.Typesystem.Char_Star;
      Handler_Id            : out Safir.Dob.Typesystem.Int_64;
      Success               : out C.char);
   pragma Import (C, Get_Handler_Id, "DoseC_GetHandlerId");

   procedure Get_Connection_Info
     (Current_State : in Safir.Dob.Typesystem.Char_Star;
      Blob          : out Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter  : out Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Success       : out C.char);
   pragma Import (C, Get_Connection_Info, "DoseC_GetConnectionInfo");

   procedure Get_Top_Timestamp
     (State         : in Safir.Dob.Typesystem.Char_Star;
      Timestamp     : out Safir.Dob.Typesystem.Int_64;
      Success       : out C.char);
   pragma Import (C, Get_Top_Timestamp, "DoseC_GetTopTimestamp");

   procedure Get_Member_Timestamp
     (State         : in Safir.Dob.Typesystem.Char_Star;
      Member        : in Safir.Dob.Typesystem.Member_Index;
      Timestamp     : out Safir.Dob.Typesystem.Int_64;
      Success       : out C.char);
   pragma Import (C, Get_Member_Timestamp, "DoseC_GetMemberTimestamp");

   procedure Get_Queue_Capacity
     (Controller_Id   : in Safir.Dob.Defs.Controller_Id;
      Queue           : in Safir.Dob.Typesystem.Enumeration_Value;
      Queue_Capacity  : out Safir.Dob.Typesystem.Int_32;
      Success         : out C.char);
   pragma Import (C, Get_Queue_Capacity, "DoseC_GetQueueCapacity");

   procedure Get_Queue_Size
     (Controller_Id   : in Safir.Dob.Defs.Controller_Id;
      Queue           : in Safir.Dob.Typesystem.Enumeration_Value;
      Queue_Size      : out Safir.Dob.Typesystem.Int_32;
      Success         : out C.char);
   pragma Import (C, Get_Queue_Size, "DoseC_GetQueueSize");

   procedure Diff
     (Previous_State : in Safir.Dob.Typesystem.Char_Star;
      Current_State  : in Safir.Dob.Typesystem.Char_Star;
      Want_Current   : in C.char;
      Timestamp_Diff : in C.char;
      Diff_Blob      : out Safir.Dob.Typesystem.Blob_T;
      Blob_Deleter   : out Safir.Dob.Blob_References.Blob_Deleter_Proc;
      Success        : out C.char);
   pragma Import (C, Diff, "DoseC_Diff");

   procedure Add_Reference
     (Ptr : in Safir.Dob.Typesystem.Char_Star);
   pragma Import (C, Add_Reference, "DoseC_AddReference");

   procedure Drop_Reference
     (Ptr : in Safir.Dob.Typesystem.Char_Star);
   pragma Import (C, Drop_Reference, "DoseC_DropReference");

   ---------------------------
   -- EntityIterator functions
   ---------------------------
   -- destroy needs to be called even if end is true!

   procedure Entity_Iterator_Create
     (Controller_Id     : in Safir.Dob.Defs.Controller_Id;
      Type_Id           : in Safir.Dob.Typesystem.Type_Id;
      Include_Subsclass : in C.char;
      Iterator_Id       : out Safir.Dob.Typesystem.Int_32;
      It_End            : out C.char;
      Success           : out C.char);
   pragma Import (C, Entity_Iterator_Create, "DoseC_EntityIteratorCreate");

   procedure Entity_Iterator_Destroy
     (Controller_Id     : in Safir.Dob.Defs.Controller_Id;
      Iterator_Id       : in Safir.Dob.Typesystem.Int_32);
   pragma Import (C, Entity_Iterator_Destroy, "DoseC_EntityIteratorDestroy");

   procedure Entity_Iterator_Copy
     (Controller_Id     : in Safir.Dob.Defs.Controller_Id;
      Iterator_Id       : in Safir.Dob.Typesystem.Int_32;
      Iterator_Id_Copy  : out Safir.Dob.Typesystem.Int_32;
      Success           : out C.char);
   pragma Import (C, Entity_Iterator_Copy, "DoseC_EntityIteratorCopy");

   procedure Entity_Iterator_Increment
     (Controller_Id     : in Safir.Dob.Defs.Controller_Id;
      Iterator_Id       : in Safir.Dob.Typesystem.Int_32;
      It_End            : out C.char;
      Success           : out C.char);
   pragma Import (C, Entity_Iterator_Increment, "DoseC_EntityIteratorIncrement");

   procedure Entity_Iterator_Dereference
     (Controller_Id     : in Safir.Dob.Defs.Controller_Id;
      Iterator_Id       : in Safir.Dob.Typesystem.Int_32;
      Entity_Blob       : out Safir.Dob.Typesystem.Blob_T;
      Entity_State      : out Safir.Dob.Typesystem.Char_Star;
      Success           : out C.char);
   pragma Import (C, Entity_Iterator_Dereference, "DoseC_EntityIteratorDereference");

   procedure Entity_Iterator_Equal
     (Controller_Id     : in Safir.Dob.Defs.Controller_Id;
      First             : in Safir.Dob.Typesystem.Int_32;
      Second            : in Safir.Dob.Typesystem.Int_32;
      Equal             : out C.char;
      Success           : out C.char);
   pragma Import (C, Entity_Iterator_Equal, "DoseC_EntityIteratorEqual");

   procedure Simulate_Overflows
     (Controller_Id : in Safir.Dob.Defs.Controller_Id;
      In_Queues     : in C.char;
      Out_Queues    : in C.char;
      Success       : out C.char);
   pragma Import (C, Simulate_Overflows, "DoseC_SimulateOverflows");

end Safir.Dob.Interf;
