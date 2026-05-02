with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Characters.Handling;
with GNAT.OS_Lib;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

use Ada.Text_IO;        -- Put_Line, Standard_Error
use Ada.Directories;    -- Necessary to get the = operator
use Ada.Strings.Fixed;  -- Index

procedure Which is
   package Exc renames Ada.Exceptions;
   package Tio renames Ada.Text_IO;
   package UnbTio renames Ada.Strings.Unbounded.Text_IO;
   package Env renames Ada.Environment_Variables;
   package Dirs renames Ada.Directories;
   package CharHnd renames Ada.Characters.Handling;
   package FxdStr renames Ada.Strings.Fixed;
   package CmdLine renames Ada.Command_Line;
   package OS renames GNAT.OS_Lib;
   package UnbStr renames Ada.Strings.Unbounded;
   package StrVect is new Ada.Containers.Indefinite_Vectors
     (Element_Type => String, Index_Type => Positive);

   use type StrVect.Vector;

   -- VAR
   find_all         : Boolean := False;
   verbose          : Boolean := False;
   substring_search : Boolean := False;
   path_dirs        : StrVect.Vector;
   exts             : StrVect.Vector;
   path_sep         : String(1..1);

   function Get_Environment_Variable (name : in String) return String is
      Env_Var_Not_Found : exception;
      e : Exc.Exception_Occurrence;
   begin
      return Env.Value (name);
   exception
      when e : others =>
         raise Env_Var_Not_Found with Exc.Exception_Message (e) & ": " & name;
   end Get_Environment_Variable;

   function To_Lower_Case (S : in String) return String is
      Lower_Cased_S : String (S'Range);
   begin
      for i in S'Range loop
         Lower_Cased_S (i) := CharHnd.To_Lower (S (i));
      end loop;
      return Lower_Cased_S;
   end To_Lower_Case;

   function Ends_With(S, Ending : String) return Boolean is
      First_Char : Positive;
      Result : Boolean;
   begin
      if S'Length >= Ending'Length then -- Avoid negative index
         First_Char := S'Last - Ending'Length + 1;
         Result := S(First_Char..S'Last) = Ending;
      else
         Result := False;
      end if;
      return Result;
   end Ends_With;

   function Is_Executable (File : Dirs.Directory_Entry_Type) return Boolean is
      -- Uses global variable: Exts
      Extension_Matches : Boolean := False;
      Is_Executable     : Boolean;

      procedure Compare_File_Extension_To (Exts_Cursor : StrVect.Cursor) is
      begin
         if Ends_With(Dirs.Simple_Name(File), StrVect.Element(Exts_Cursor))
         then
            Extension_Matches := True;
         end if;
      end Compare_File_Extension_To;

   begin
      if StrVect.Is_Empty (exts) then
         -- UNIX has no PATHEXT environment variable so Exts will be empty.
         -- Is_Executable := OS.Is_Executable_File(dirs.Full_Name(File));
         Is_Executable := True;
      else
         -- On Windows the PATHEXT variable is needed.
         StrVect.Iterate (exts, Compare_File_Extension_To'Access);
         Is_Executable := Extension_Matches;
      end if;
      return Is_Executable;
   end Is_Executable;

   procedure Split
     (Txt   : in  String;
      Delim : in  String;
      Parts : out StrVect.Vector)
   is
      Delim_Idx : Natural;
      Start_Idx : Positive := Positive'First;
      Part      : String;
   begin
      Parts := StrVect.Empty_Vector;
      Delim_Idx := Index(Txt, Delim, Start_Idx);
      while Delim_Idx > 0 loop
         Part := Txt(Start_Idx..Delim_Idx - 1);
         StrVect.Append(Parts, Part);
         Start_Idx := Delim_Idx + Delim'Length;
         Delim_Idx := Index(Txt, Delim, Start_Idx);
      end loop;
      -- Get remainder part.
      if Start_Idx <= Txt'Length then
         Part := Txt(Start_Idx..Txt'Length);
         StrVect.Append(Parts, Part);
      end if;
   end Split;

   function Is_Directory (Dir_Name : in String) return Boolean is
      e : Exc.Exception_Occurrence;
   begin
      return Dirs.Kind (Dir_Name) = Dirs.Directory;
   exception
         -- Name_Error is raised when the file is not found
      when e : Dirs.Name_Error =>
         if verbose then
            Put_Line ("Directory not found: " & Dir_Name);
         end if;
         return False;
      when e : others =>
         raise;
   end Is_Directory;

   procedure Find_One_Program_In_Path
     (Dirs_From_Path : in StrVect.Vector; Search_Term : in String)
   is

      procedure Search_Directory (Directories_Cursor : StrVect.Cursor) is

         procedure Check_If_Dir_Entry_Matches_Search_Term
           (Dir_Entry : Dirs.Directory_Entry_Type)
         is
            matches            : Boolean := False;
            searchTermPosition : Natural;
         begin
            if substring_search then
               searchTermPosition :=
                 FxdStr.Index (Dirs.simple_name (Dir_Entry), Search_Term, 1);
               if searchTermPosition > 0 and then Is_Executable (Dir_Entry)
               then
                  Put_Line (Dirs.Full_Name (Dir_Entry));
               end if;
            elsif Search_Term = Dirs.Simple_Name (Dir_Entry)
              and then Is_Executable (Dir_Entry)
            then
               Put_Line (Dirs.Full_Name (Dir_Entry));
            end if;
         end Check_If_Dir_Entry_Matches_Search_Term;

         -- VAR
         Dir_Name : String := StrVect.Element (Directories_Cursor);
      begin
         if verbose then
            Put_Line ("Checking directory: " & Dir_Name);
         end if;

         if Is_Directory (Dir_Name) then
            -- Call Check_... for each file in Dir_Name directory.
            Dirs.Search
              (Dir_Name, "", (others => true),
               Check_If_Dir_Entry_Matches_Search_Term'Access);
         end if;
      end Search_Directory;

   begin
      -- Iterate over all entries in Dirs_From_Path, calling
      -- Search_Directory for each one. The cursor denoting the current
      -- position in Dirs_From_Path is passed to Search_Directory.
      StrVect.Iterate (Dirs_From_Path, Search_Directory'Access);
   end Find_One_Program_In_Path;

   --
   -- Main Program
   --
   e : Exc.Exception_Occurrence;
begin
   if Env.Exists ("PATHEXT") then
      -- Running on Windoze
      path_sep := ";";
      split(exts, path_sep, Get_Environment_Variable("PATHEXT"));
   else
      -- Running on a UNIX-Like system
      path_sep := ":";
   end if;

   split(path_dirs, path_sep, Get_Environment_Variable("PATH"));

   -- If there are some command line arguments
   if CmdLine.Argument_Count > 0 then
      for i in 1 .. CmdLine.Argument_Count loop
         if CmdLine.Argument(I) = "-a" then
            find_all := True;
         elsif CmdLine.Argument(I) = "-v" then
            verbose := True;
         elsif CmdLine.Argument(I) = "-s" then
            substring_search := True;
         else
            Find_One_Program_In_Path(Path_Dirs, CmdLine.Argument(I));
         end if;
      end loop;
   else
      Put_Line ("No arguments");
   end if;

exception
   when e : others =>
      Put_Line (Standard_Error, "Exception: " & Exc.Exception_Message (e));
end Which;
