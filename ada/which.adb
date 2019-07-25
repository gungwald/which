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

use Ada.Strings.Unbounded; -- Necessary to get the & and = operators...
use Ada.Exceptions;

procedure which is

    package tio  renames Ada.Text_IO;
    package uio  renames Ada.Strings.Unbounded.Text_IO;
    package env  renames Ada.Environment_Variables;
    package dirs renames Ada.Directories;
    package char renames Ada.Characters.Handling;
    package fstr renames Ada.Strings.Fixed;
    package args renames Ada.Command_Line;
    package os   renames GNAT.OS_Lib;

    package ustr renames Ada.Strings.Unbounded;
    package ustr_vect is new Ada.Containers.Vectors(element_type => ustr.Unbounded_String, index_type => Natural, "=" => ustr."=");


    find_all         : Boolean := False;
    verbose          : Boolean := False;
    substring_search : Boolean := False;
    path_dirs        : ustr_vect.Vector;
    exts             : ustr_vect.Vector;
    path_sep         : ustr.Unbounded_String;


    function Get_Environment_Variable(name: in String) return Unbounded_String is
        Environment_Variable_Not_Found : exception;
        e : Exception_Occurrence;
    begin
        return To_Unbounded_String(Env.Value(name));
    exception
        when e : others =>
            raise Environment_Variable_Not_Found with Exception_Message(e) & ": " & name;
    end Get_Eironment_Variable;


    function To_Lower_Case(s : Unbounded_String) return Unbounded_String is
        lowered_str : Unbounded_String;
    begin
        lowered_str := to_unbounded_string("");
        for i in 1..length(s) loop
            append(lowered_str, char.to_lower(element(s, i)));
        end loop;
        return lowered_str;
    end To_Lower_Case;


    function Ends_With(s: Unbounded_String; Suffix_To_Match: Unbounded_String) return Boolean is
	Ends_With: Boolean;
	Length_Of_Suffix_To_Match: Natural;
	End_Of_S: Unbounded_String;
    begin
	Length_Of_Suffix_To_Match := Length(Suffix_To_Match);
	End_Of_S := 
        if To_Lower_Case(Tail(s, Length(Suffix_To_Match))) = To_Lower_Case(Suffix_To_Match) then
	    Ends_With := true;
	else
	    Ends_With := false;
        end if;
    end Ends_With;


    function indexOf(search_in : Unbounded_String; search_for : Unbounded_String; start_position : Positive) return Natural is
    begin
        return index(search_in, to_string(search_for), start_position);
    end indexOf;



    function Is_Executable(File : dirs.Directory_Entry_Type) return Boolean is
	-- Uses global variable: Exts
        Extension_Matches : Boolean := False;
	Is_Executable : Boolean;

        procedure Compare_File_Extension_To(Exts_Cursor : ustr_vect.Cursor) is
        begin
            if Ends_With(To_Unbounded_String(dirs.Simple_Name(File)), ustr_vect.Element(Exts_Cursor)) then
                Extension_Matches := True;
            end if;
        end Compare_File_Extension_To;
            
    begin
        if ustr_vect.Is_Empty(Exts) then
            -- UNIX has no PATHEXT environment variable so Exts will be empty.
            Is_Executable := Os.Is_Executable_File(dirs.Full_Name(File)); 
	else
            -- On Windows the PATHEXT variable is needed.
            ustr_vect.Iterate(Exts, Compare_File_Extension_To'Access);
	    Is_Executable := Extension_Matches;
        end if;
        return Is_Executable;
    end Is_Executable;


    procedure split(parts : out ustr_vect.Vector;  splittor : in Unbounded_String;  to_split : in Unbounded_String) is
        splittor_position : Natural;
        startFrom : Positive := Positive'first;
    begin
        ustr_vect.clear(parts);
        splittor_position := indexOf(to_split, splittor, startFrom);
        while splittor_position > 0 loop
            ustr_vect.append(parts, unbounded_slice(to_split, startFrom, splittor_position - 1));
            startFrom := splittor_position + length(splittor);
            splittor_position := indexOf(to_split, splittor, startFrom);
        end loop;
        if startFrom <= length(to_split) then
            ustr_vect.append(parts, unbounded_slice(to_split, startFrom, length(to_split)));
        end if;
    end split;


    function isDirectory(d : Unbounded_String) return Boolean is
        dir_name : String(1..length(d)) := to_string(d);
    begin
        return dirs.exists(dir_name) and ( dirs.file_kind'pos(dirs.kind(dir_name)) = dirs.file_kind'pos(dirs.DIRECTORY) );
    end isDirectory;


    procedure which(searchTerm : String) is
        
        procedure searchInDirectory(directoriesCursor : ustr_vect.Cursor) is

            procedure Check_File(Dir_Entry : dirs.Directory_Entry_Type) is
                matches : Boolean := false;
                searchTermPosition : Natural;
            begin
                if substring_search then
                    searchTermPosition := fstr.index(dirs.simple_name(Dir_Entry), searchTerm, 1);
                    if searchTermPosition > 0 and Is_Executable(Dir_Entry) then
                        tio.put_line(dirs.full_name(Dir_Entry));
                    end if;
                elsif searchTerm = dirs.simple_name(Dir_Entry) and Is_Executable(Dir_Entry) then
                    tio.put_line(dirs.full_name(Dir_Entry));
                end if;
            end Check_File;
        
        begin
            if verbose then
                uio.put_line("Checking dir: " & ustr_vect.element(directoriesCursor));
            end if;

            if isDirectory(ustr_vect.element(directoriesCursor)) then
                dirs.search(to_string(ustr_vect.element(directoriesCursor)), "", (others => true), Check_File'access);
            else
                if verbose then
                    uio.put_line("Directory not found: " & ustr_vect.element(directoriesCursor));
                end if;
            end if;
        end searchInDirectory;

    begin
        ustr_vect.iterate(path_dirs, searchInDirectory'access);
    end which;

--
-- Main Program
--
    e : Exception_Occurrence;
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
        tio.put_line(tio.Standard_Error, "Exception: " & Exception_Message(e));
end which;

