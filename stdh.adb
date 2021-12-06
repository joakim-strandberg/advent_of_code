------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--           B A S I C   B O U N D E D   D Y N A M I C   P O O L S
--
--                                B o d y
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Deepend is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Paraffin is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Deepend;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Stdh is

   package body Basic_Bounded_Dynamic_Pools is

      procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
        (Object => Storage_Array,
         Name => Storage_Array_Access);

      --------------------------------------------------------------

      procedure Allocate (Pool : in out Basic_Dynamic_Pool;
                          Storage_Address : out Address;
                          Size_In_Storage_Elements : Storage_Elements.Storage_Count;
                          Alignment : Storage_Elements.Storage_Count)
      is
      begin

         --        pragma Assert (Is_Owner (Pool, Current_Task));

         if Size_In_Storage_Elements >
           Pool.Active'Length - Pool.Next_Allocation
         then

            raise Storage_Error;
         end if;

         Storage_Address := Pool.Active (Pool.Next_Allocation)'Address;

         Pool.Next_Allocation := Pool.Next_Allocation + Size_In_Storage_Elements;

      end Allocate;

      --------------------------------------------------------------

      procedure Deallocate
        (Pool : in out Basic_Dynamic_Pool;
         Storage_Address : Address;
         Size_In_Storage_Elements : Storage_Elements.Storage_Count;
         Alignment : Storage_Elements.Storage_Count) is
      begin
         null;
      end Deallocate;

      --------------------------------------------------------------

      procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
      begin
         null;
      end Finalize;

      --------------------------------------------------------------

      procedure Initialize (Pool : in out Basic_Dynamic_Pool) is
      begin
         Pool.Next_Allocation := 1;
         Pool.Owner := Ada.Task_Identification.Current_Task;
      end Initialize;

      --------------------------------------------------------------

      function Is_Owner
        (Pool : Basic_Dynamic_Pool;
         T : Task_Id := Current_Task) return Boolean is
      begin
         return (Pool.Owner = T);
      end Is_Owner;

      --------------------------------------------------------------

      procedure Set_Owner
        (Pool : in out Basic_Dynamic_Pool;
         T : Task_Id := Current_Task) is
      begin
         --        pragma Assert
         --          ((Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
         --           or else (Is_Owner (Pool) and then T = Null_Task_Id));

         Pool.Owner := T;

      end Set_Owner;

      --------------------------------------------------------------

      function Storage_Size
        (Pool : Basic_Dynamic_Pool)
         return Storage_Elements.Storage_Count is
      begin
         return Pool.Size;
      end Storage_Size;

      --------------------------------------------------------------

      function Storage_Used
        (Pool : Basic_Dynamic_Pool)
         return Storage_Elements.Storage_Count is
      begin
         return Pool.Next_Allocation - 1;
      end Storage_Used;

   end Basic_Bounded_Dynamic_Pools;

   package body Unbounded_Bounded_Vectors is

      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Elements_Array,
         Name   => Elements_Ptr);

      procedure Initialize (Container : in out Vector) is
      begin
         if Container.Items = null then
            Container.Items := new Elements_Array;
         end if;
      end Initialize;

      procedure Finalize (Container : in out Vector) is
      begin
         Free (Container.Items);
      end Finalize;

      function First_Index return Index_Type is
      begin
         return Index_Type'First;
      end First_Index;

      function Last_Index (This : Vector) return Extended_Index is
      begin
         return This.My_Last;
      end Last_Index;

      function Is_Empty (This : Vector) return Boolean is
      begin
         return This.My_Last = Extended_Index'First;
      end Is_Empty;

      function Is_Full (This : Vector) return Boolean is
      begin
         return This.My_Last = Index_Type'Last;
      end Is_Full;

      function "=" (L, R : Vector) return Boolean is
         Result : Boolean := True;
      begin
         if Last_Index (L) = Last_Index (R) then
            for I in Index_Type range Index_Type'First .. Last_Index (L) loop
               if L.Items (I) /= R.Items (I) then
                  Result := False;
                  exit;
               end if;
            end loop;
         else
            Result := False;
         end if;

         return Result;
      end "=";

      procedure Append (This     : in out Vector;
                        New_Item : Element_Type) is
      begin
         This.My_Last := This.My_Last + 1;
         This.Items (Index_Type (This.My_Last)) := New_Item;
      end Append;

      function Contains (This         : Vector;
                         Searched_For : Element_Type) return Boolean
      is
         Result : Boolean := False;
      begin
         for I in Extended_Index range Index_Type'First .. This.My_Last loop
            if This.Items (I) = Searched_For then
               Result := True;
               exit;
            end if;
         end loop;
         return Result;
      end Contains;

      function Element
        (This  : Vector;
         Idx   : Index_Type) return Element_Type is
      begin
         return This.Items (Idx);
      end Element;

      function Last_Element (This : Vector) return Element_Type is
      begin
         return This.Items (Index_Type (This.My_Last));
      end Last_Element;

      procedure Delete_Last (This : in out Vector) is
      begin
         This.My_Last := This.My_Last - 1;
      end Delete_Last;

      procedure Delete (This : in out Vector;
                        Item : Element_Type) is
      begin
         for I in Index_Type range Index_Type'First .. This.My_Last loop
            if This.Items (I) = Item then
               This.Items (I .. This.My_Last - 1)
                 := This.Items (I + 1 .. This.My_Last);
               This.My_Last := This.My_Last - 1;
            end if;
         end loop;
      end Delete;

      procedure Clear (This : in out Vector) is
      begin
         This.My_Last := Extended_Index'First;
      end Clear;

      procedure Replace_Element
        (This        : in out Vector;
         Idx         : Index_Type;
         New_Element : Element_Type) is
      begin
         This.Items (Idx) := New_Element;
      end Replace_Element;

      procedure Replace_Last_Element
        (This        : in out Vector;
         New_Element : Element_Type) is
      begin
         This.Items (Last_Index (This)) := New_Element;
      end Replace_Last_Element;

      function Length (This : Vector) return Nat32 is
      begin
         return Nat32 (This.My_Last);
      end Length;

   end Unbounded_Bounded_Vectors;

end Stdh;
