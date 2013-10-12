-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
with Safir.Dob.Typesystem;
with Safir.Dob.Consumer_Bases;
with Safir.Dob.Defs;
with Interfaces.C;
with Interfaces.C.Strings;

package Safir.Dob.Callbacks is

   package C renames Interfaces.C;

   -----------------------
   -- On_Do_Dispatch_Cb --
   -----------------------

   type On_Do_Dispatch_Cb_Ptr is
     access procedure
       (Consumer : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success  : out C.char);
   pragma Convention (C, On_Do_Dispatch_Cb_Ptr);

   procedure On_Do_Dispatch_Cb
     (Consumer : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success    : out C.char);
   pragma Convention (C, On_Do_Dispatch_Cb);

   ----------------------
   -- On_Stop_Order_Cb --
   ----------------------

   type On_Stop_Order_Cb_Ptr is
     access procedure
       (Consumer : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success  :    out C.char);
   pragma Convention (C, On_Stop_Order_Cb_Ptr);

   procedure On_Stop_Order_Cb
     (Consumer : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success  : out C.char);
   pragma Convention (C, On_Stop_Order_Cb);

   ----------------------
   -- On_New_Entity_Cb --
   ----------------------

   type On_New_Entity_Cb_Ptr is
     access procedure
       (Current_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Current_State     : in Safir.Dob.Typesystem.Char_Star;
        Consumer          : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff    : in C.char;
        Success           : out C.char);
   pragma Convention (C, On_New_Entity_Cb_Ptr);

   procedure On_New_Entity_Cb
       (Current_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Current_State     : in Safir.Dob.Typesystem.Char_Star;
        Consumer          : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff    : in C.char;
        Success           : out C.char);
   pragma Convention (C, On_New_Entity_Cb);

   --------------------------
   -- On_Updated_Entity_Cb --
   --------------------------

   type On_Updated_Entity_Cb_Ptr is
     access procedure
       (Current_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Current_State      : in Safir.Dob.Typesystem.Char_Star;
        Previous_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Previous_State     : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff     : in C.char;
        Success            : out C.char);
   pragma Convention (C, On_Updated_Entity_Cb_Ptr);

   procedure On_Updated_Entity_Cb
       (Current_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Current_State      : in Safir.Dob.Typesystem.Char_Star;
        Previous_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Previous_State     : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff     : in C.char;
        Success            : out C.char);
   pragma Convention (C, On_Updated_Entity_Cb);


   --------------------------
   -- On_Deleted_Entity_Cb --
   --------------------------

   type On_Deleted_Entity_Cb_Ptr is
     access procedure
       (Current_State      : in Safir.Dob.Typesystem.Char_Star;
        Previous_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Previous_State     : in Safir.Dob.Typesystem.Char_Star;
        Explicitly_Deleted : in C.char;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff     : in C.char;
        Success            : out C.char);
   pragma Convention (C, On_Deleted_Entity_Cb_Ptr);

   procedure On_Deleted_Entity_Cb
       (Current_State      : in Safir.Dob.Typesystem.Char_Star;
        Previous_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Previous_State     : in Safir.Dob.Typesystem.Char_Star;
        Explicitly_Deleted : in C.char;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Timestamp_Diff     : in C.char;
        Success            : out C.char);
   pragma Convention (C, On_Deleted_Entity_Cb);

   --------------------------
   -- On_Create_Request_Cb --
   --------------------------

   type On_Create_Request_Cb_Ptr is
     access procedure
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Create_Request_Cb_Ptr);

   procedure On_Create_Request_Cb
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Create_Request_Cb);

   --------------------------
   -- On_Update_Request_Cb --
   --------------------------

   type On_Update_Request_Cb_Ptr is
     access procedure
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Update_Request_Cb_Ptr);

   procedure On_Update_Request_Cb
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Update_Request_Cb);

   --------------------------
   -- On_Delete_Request_Cb --
   --------------------------

   type On_Delete_Request_Cb_Ptr is
     access procedure
     (State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Delete_Request_Cb_Ptr);

   procedure On_Delete_Request_Cb
     (State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Delete_Request_Cb);

   --------------------------
   -- On_Service_Request_Cb --
   --------------------------

   type On_Service_Request_Cb_Ptr is
     access procedure
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Service_Request_Cb_Ptr);

   procedure On_Service_Request_Cb
     (Request_Blob               : in Safir.Dob.Typesystem.Blob_T;
      State                      : in Safir.Dob.Typesystem.Char_Star;
      Controller_Id              : in Safir.Dob.Defs.Controller_Id;
      Response_Id                : in Safir.Dob.Defs.Response_Id;
      Consumer                   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                    : out C.char);
   pragma Convention (C, On_Service_Request_Cb);

   --------------------
   -- On_Response_Cb --
   --------------------

   type On_Response_Cb_Ptr is
     access procedure
       (Request_Id         : in Safir.Dob.Defs.Request_Id;
        Response_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Response_State     : in Safir.Dob.Typesystem.Char_Star;
        Request_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Request_State      : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success            : out C.char);
   pragma Convention (C, On_Response_Cb_Ptr);

   procedure On_Response_Cb
       (Request_Id         : in Safir.Dob.Defs.Request_Id;
        Response_Blob      : in Safir.Dob.Typesystem.Blob_T;
        Response_State     : in Safir.Dob.Typesystem.Char_Star;
        Request_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Request_State      : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success            : out C.char);
   pragma Convention (C, On_Response_Cb);

   --------------------
   -- On_Message_Cb --
   --------------------

   type On_Message_Cb_Ptr is
     access procedure
       (Message_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Message_State      : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success            : out C.char);
   pragma Convention (C, On_Message_Cb_Ptr);

   procedure On_Message_Cb
       (Message_Blob       : in Safir.Dob.Typesystem.Blob_T;
        Message_State      : in Safir.Dob.Typesystem.Char_Star;
        Consumer           : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success            : out C.char);
   pragma Convention (C, On_Message_Cb);

   ----------------------
   -- On_Registered_Cb --
   ----------------------

   type On_Registered_Cb_Ptr is
     access procedure
       (Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id              : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str          : in C.Strings.chars_ptr;
        Consumer                : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                 : out C.char);
   pragma Convention (C, On_Registered_Cb_Ptr);

   procedure On_Registered_Cb
       (Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id              : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str          : in C.Strings.chars_ptr;
        Consumer                : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                 : out C.char);
   pragma Convention (C, On_Registered_Cb);

   ------------------------
   -- On_Unregistered_Cb --
   ------------------------

   type On_Unregistered_Cb_Ptr is
     access procedure
       (Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id              : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str          : in C.Strings.chars_ptr;
        Consumer                : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                 : out C.char);
   pragma Convention (C, On_Unregistered_Cb_Ptr);

   procedure On_Unregistered_Cb
       (Type_Id                 : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id              : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str          : in C.Strings.chars_ptr;
        Consumer                : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                 : out C.char);
   pragma Convention (C, On_Unregistered_Cb);

   --------------------------------
   -- On_Revoked_Registration_Cb --
   --------------------------------

   type On_Revoked_Registration_Cb_Ptr is
     access procedure
       (Type_Id                   : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id                : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str            : in C.Strings.chars_ptr;
        Consumer                  : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                   : out C.char);
   pragma Convention (C, On_Revoked_Registration_Cb_Ptr);

   procedure On_Revoked_Registration_Cb
     (Type_Id                   : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id                : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str            : in C.Strings.chars_ptr;
      Consumer                  : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                   : out C.char);
   pragma Convention (C, On_Revoked_Registration_Cb);

   ----------------------------------
   -- On_Completed_Registration_Cb --
   ----------------------------------

   type On_Completed_Registration_Cb_Ptr is
     access procedure
       (Type_Id                     : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id                  : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str              : in C.Strings.chars_ptr;
        Consumer                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                     : out C.char);
   pragma Convention (C, On_Completed_Registration_Cb_Ptr);

   procedure On_Completed_Registration_Cb
     (Type_Id                     : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id                  : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str              : in C.Strings.chars_ptr;
      Consumer                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                     : out C.char);
   pragma Convention (C, On_Completed_Registration_Cb);

   -------------------------------
   -- On_Injected_New_Entity_Cb --
   -------------------------------

   type On_Injected_New_Entity_Cb_Ptr is
     access procedure
       (Injection_Blob        : in Safir.Dob.Typesystem.Blob_T;
        Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char);
   pragma Convention (C, On_Injected_New_Entity_Cb_Ptr);

   procedure On_Injected_New_Entity_Cb
       (Injection_Blob        : in Safir.Dob.Typesystem.Blob_T;
        Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char);
   pragma Convention (C, On_Injected_New_Entity_Cb);

   -----------------------------------
   -- On_Injected_Updated_Entity_Cb --
   -----------------------------------

   type On_Injected_Updated_Entity_Cb_Ptr is
     access procedure
       (Injection_Blob        : in Safir.Dob.Typesystem.Blob_T;
        Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
        Current_State         : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char);
   pragma Convention (C, On_Injected_Updated_Entity_Cb_Ptr);

   procedure On_Injected_Updated_Entity_Cb
       (Injection_Blob        : in Safir.Dob.Typesystem.Blob_T;
        Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
        Current_State         : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char);
   pragma Convention (C, On_Injected_Updated_Entity_Cb);

   -------------------------------
   -- On_Injected_Deleted_Entity_Cb --
   -------------------------------

   type On_Injected_Deleted_Entity_Cb_Ptr is
     access procedure
       (Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
        Current_State         : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char);
   pragma Convention (C, On_Injected_Deleted_Entity_Cb_Ptr);

   procedure On_Injected_Deleted_Entity_Cb
       (Injection_State       : in Safir.Dob.Typesystem.Char_Star;
        Current_Blob          : in Safir.Dob.Typesystem.Blob_T;
        Current_State         : in Safir.Dob.Typesystem.Char_Star;
        Consumer              : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success               : out C.char);
   pragma Convention (C, On_Injected_Deleted_Entity_Cb);

   -----------------------------------
   -- On_Initial_Injections_Done_Cb --
   -----------------------------------

   type On_Initial_Injections_Done_Cb_Ptr is
     access procedure
       (Type_Id                     : in Safir.Dob.Typesystem.Type_Id;
        Handler_Id                  : in Safir.Dob.Typesystem.Int_64;
        Handler_Id_Str              : in C.Strings.chars_ptr;
        Consumer                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success                     : out C.char);
   pragma Convention (C, On_Initial_Injections_Done_Cb_Ptr);

   procedure On_Initial_Injections_Done_Cb
     (Type_Id                     : in Safir.Dob.Typesystem.Type_Id;
      Handler_Id                  : in Safir.Dob.Typesystem.Int_64;
      Handler_Id_Str              : in C.Strings.chars_ptr;
      Consumer                    : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success                     : out C.char);
   pragma Convention (C, On_Initial_Injections_Done_Cb);

   --------------------------------
   -- On_Not_Request_Overflow_Cb --
   --------------------------------

   type On_Not_Request_Overflow_Cb_Ptr is
     access procedure
       (Consumer   : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success    : out C.char);
   pragma Convention (C, On_Not_Request_Overflow_Cb_Ptr);

   procedure On_Not_Request_Overflow_Cb
     (Consumer     : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success      : out C.char);
   pragma Convention (C, On_Not_Request_Overflow_Cb);

   --------------------------------
   -- On_Not_Message_Overflow_Cb --
   --------------------------------

   type On_Not_Message_Overflow_Cb_Ptr is
     access procedure
       (Consumer : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
        Success  : out C.char);
   pragma Convention (C, On_Not_Message_Overflow_Cb_Ptr);

   procedure On_Not_Message_Overflow_Cb
     (Consumer : access Safir.Dob.Consumer_Bases.Internal.Consumer_Base'Class;
      Success  : out C.char);
   pragma Convention (C, On_Not_Message_Overflow_Cb);

end Safir.Dob.Callbacks;
