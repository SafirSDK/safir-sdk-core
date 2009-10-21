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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Defs;
with Safir.Dob.Connection_Bases;
with Safir.Dob.Entity;
with Safir.Dob.Consumers;

package Safir.Dob.Connection_Aspect_Injectors is

   type Connection_Aspect_Injector is tagged private;

   function Create
     (Connection_Base : in  Safir.Dob.Connection_Bases.Connection_Base'Class)
      return Connection_Aspect_Injector;

   -- AWI:todo comment
   procedure Inject_Changes
     (Self        : in Connection_Aspect_Injector;
      Entity      : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Timestamp   : in Safir.Dob.Typesystem.Int_64;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- AWI:todo comment
   procedure Inject_Delete
     (Self        : in Connection_Aspect_Injector;
      Entity_Id   : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      Timestamp   : in Safir.Dob.Typesystem.Int_64;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- AWI:todo comment
   procedure Initial_Set
     (Self        : in Connection_Aspect_Injector;
      Entity      : in Safir.Dob.Entity.Smart_Pointer'Class;
      Instance_Id : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      Handler_Id  : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type);

   -- AWI:todo comment
   procedure Subscribe_Entity
     (Self                                 : in Connection_Aspect_Injector;
      Type_Id                              : in Safir.Dob.Typesystem.Type_Id;
      Include_Updates                      : in Boolean;
      Include_Subclasses                   : in Boolean;
      Restart_Subscription                 : in Boolean;
      Wants_Ghost_Delete                   : in Boolean;
      Wants_Last_State                     : in Boolean;
      Doesnt_Want_Source_Is_PermanentStore : in Boolean;
      Wants_All_State_Changes              : in Boolean;
      Timestamp_Change_Info                : in Boolean;
      Entity_Subscriber                    : access Safir.Dob.Consumers.Entity_Subscriber'Class);

private

   use type Safir.Dob.Defs.Controller_Id;

   type Connection_Aspect_Injector is tagged record
      Controller_Id : Safir.Dob.Defs.Controller_Id := -1;
   end record;

end Safir.Dob.Connection_Aspect_Injectors;
