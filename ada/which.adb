with Ada.Text_IO;       	
with Ada.Command_Line;  	
with Ada.Environment_Variables;	
with Ada.Strings.Fixed;
with Ada.Directories;		
with Ada.Exceptions;
with Ada.Characters.Handling;
with GNAT.OS_Lib;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

use  Ada.Strings.Unbounded; -- Necessary to get the & and = operators...

procedure which is

    package ex   renames Ada.Exceptions;
    package tio  renames Ada.Text_IO;
    package uio  renames Ada.Strings.Unbounded.Text_IO;
    package env  renames Ada.Environment_Variables;
    package dir  renames Ada.Directories;
    package char renames Ada.Characters.Handling;
    package fstr renames Ada.Strings.Fixed;
    package args renames Ada.Command_Line;
    package os   renames GNAT.OS_Lib;

    package Unb_Str renames Ada.Strings.Unbounded;
    package Unb_Str_Vect is new Ada.Containers.Vectors(element_type => Unb_Str.Unbounded_String, index_type => Natural, "=" => Unb_Str."=");


    find_all         : Boolean := False;
    verbose          : Boolean := False;
    substring_search : Boolean := False;
    path_dirs        : Unb_Str_Vect.Vector;
    exts             : Unb_Str_Vect.Vector;
    path_sep         : Unb_Str.Unbounded_String;


    function Get_Environment_Variable(name: in String) return Unb_Str.Unbounded_String is
        Environment_Variable_Not_Found : exception;
        e : Ex.Exception_Occurrence;
    begin
        return Unb_Str.To_Unbounded_String(Env.Value(name));
    exception
        when e : others =>
            raise Environment_Variable_Not_Found with Ex.Exception_Message(e) & ": " & name;
    end Get_Eironment_Variable;


    function To_Lower_Case(s : Unb_Str.Unbounded_String) return Unb_Str.Unbounded_String is
        lowered_str : Unb_Str.Unbounded_String;
    begin
        lowered_str := Unb_Str.to_unbounded_string("");
        for i in 1..Unb_Str.length(s) loop
            Unb_Str.append(lowered_str, char.to_lower(Unb_Str.element(s, i)));
        end loop;
        return lowered_str;
    end To_Lower_Case;


    function Ends_With(s: Unb_Str.Unbounded_String; Suffix_To_Match: Unb_Str.Unbounded_String) return Boolean is
	Ends_With: Boolean;
	Length_Of_Suffix_To_Match: Natural;
	End_Of_S: Unb_Str.Unbounded_String;
    begin
	Length_Of_Suffix_To_Match := Unb_Str.Length(Suffix_To_Match);
	End_Of_S := 
        if To_Lower_Case(Unb_Str.Tail(s, Unb_Str.Length(Suffix_To_Match))) = To_Lower_Case(Suffix_To_Match) then
	    Ends_With := true;
	else
	    Ends_With := false;
        end if;
    end Ends_With;


    function indexOf(search_in : Unb_Str.Unbounded_String; search_for : Unb_Str.Unbounded_String; start_position : Positive) return Natural is
    begin
        return Unb_Str.index(search_in, Unb_Str.to_string(search_for), start_position);
    end indexOf;



    function Is_Executable(File : Dir.Directory_Entry_Type) return Boolean is
	-- Uses global variable: Exts
        Extension_Matches : Boolean := False;
	Is_Executable : Boolean;

        procedure Compare_File_Extension_To(Exts_Cursor : Unb_Str_Vect.Cursor) is
        begin
            if Ends_With(Unb_Str.To_Unbounded_String(Dir.Simple_Name(File)), Unb_Str_Vect.Element(Exts_Cursor)) then
                Extension_Matches := True;
            end if;
        end Compare_File_Extension_To;
            
    begin
        if Unb_Str_Vect.Is_Empty(Exts) then
            -- UNIX has no PATHEXT environment variable so Exts will be empty.
            Is_Executable := Os.Is_Executable_File(Dir.Full_Name(File)); 
	else
            -- On Windows the PATHEXT variable is needed.
            Unb_Str_Vect.Iterate(Exts, Compare_File_Extension_To'Access);
	    Is_Executable := Extension_Matches;
        end if;
        return Is_Executable;
    end Is_Executable;


    procedure split(parts : out Unb_Str_Vect.Vector;  splittor : in Unb_Str.Unbounded_String;  to_split : in Unb_Str.Unbounded_String) is
        splittor_position : Natural;
        startFrom : Positive := Positive'first;
    begin
        Unb_Str_Vect.clear(parts);
        splittor_position := indexOf(to_split, splittor, startFrom);
        while splittor_position > 0 loop
            Unb_Str_Vect.append(parts, Unb_Str.unbounded_slice(to_split, startFrom, splittor_position - 1));
            startFrom := splittor_position + Unb_Str.length(splittor);
            splittor_position := indexOf(to_split, splittor, startFrom);
        end loop;
        if startFrom <= Unb_Str.length(to_split) then
            Unb_Str_Vect.append(parts, Unb_Str.unbounded_slice(to_split, startFrom, Unb_Str.length(to_split)));
        end if;
    end split;


    function isDirectory(d : Unb_Str.Unbounded_String) return Boolean is
        dir_name : String(1..Unb_Str.length(d)) := Unb_Str.to_string(d);
    begin
        return dir.exists(dir_name) and ( dir.file_kind'pos(dir.kind(dir_name)) = dir.file_kind'pos(dir.DIRECTORY) );
    end isDirectory;


    procedure which(searchTerm : String) is
        
        procedure searchInDirectory(directoriesCursor : Unb_Str_Vect.Cursor) is

            procedure Check_File(Dir_Entry : dir.Directory_Entry_Type) is
                matches : Boolean := false;
                searchTermPosition : Natural;
            begin
                if substring_search then
                    searchTermPosition := fstr.index(dir.simple_name(Dir_Entry), searchTerm, 1);
                    if searchTermPosition > 0 and Is_Executable(Dir_Entry) then
                        tio.put_line(dir.full_name(Dir_Entry));
                    end if;
                elsif searchTerm = dir.simple_name(Dir_Entry) and Is_Executable(Dir_Entry) then
                    tio.put_line(dir.full_name(Dir_Entry));
                end if;
            end Check_File;
        
        begin
            if verbose then
                uio.put_line("Checking dir: " & Unb_Str_Vect.element(directoriesCursor));
            end if;

            if isDirectory(Unb_Str_Vect.element(directoriesCursor)) then
                dir.search(to_string(Unb_Str_Vect.element(directoriesCursor)), "", (others => true), Check_File'access);
            else
                if verbose then
                    uio.put_line("Directory not found: " & Unb_Str_Vect.element(directoriesCursor));
                end if;
            end if;
        end searchInDirectory;

    begin
        Unb_Str_Vect.iterate(path_dirs, searchInDirectory'access);
    end which;

--
-- Main Program
--
    e : ex.Exception_Occurrence;
begin
    if env.Exists("PATHEXT") then
        path_sep := to_unbounded_string(";");
        split(exts, path_sep, Get_Environment_Variable("PATHEXT"));
    else
        path_sep := to_unbounded_string(":");
    end if;

    split(path_dirs, path_sep, Get_Environment_Variable("PATH"));

    -- If there are some command line arguments
    if args.Argument_Count > 0 then
        for i in 1..args.Argument_Count loop
            if args.Argument(I) = "-a" then
                Find_All := True;
            elsif args.Argument(I) = "-v" then
                Verbose := True;
            elsif args.Argument(I) = "-s" then
                Substring_Search := True;
            else
                Which(args.Argument(I));
            end if;
        end loop;
    else
        tio.put_line("No arguments");
    end if;

exception
    when e : others =>
        tio.put_line(tio.Standard_Error, "Exception: " & ex.Exception_Message(e));
end which;

