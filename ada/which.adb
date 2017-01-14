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
    package ustr renames Ada.Strings.Unbounded;
    package args renames Ada.Command_Line;
    package os   renames GNAT.OS_Lib;

    package ustr_vect is new Ada.Containers.Vectors(element_type => ustr.Unbounded_String, index_type => Natural, "=" => ustr."=");


    find_all         : Boolean := False;
    verbose          : Boolean := False;
    substring_search : Boolean := False;
    path_dirs        : ustr_vect.Vector;
    exts             : ustr_vect.Vector;
    path_sep         : ustr.Unbounded_String;


    function getenv(name : in String) return ustr.Unbounded_String is
        EnvVarNotFoundException : exception;
        e : ex.Exception_Occurrence;
    begin
        return ustr.to_unbounded_string(env.value(name));
    exception
        when e : others =>
            raise EnvVarNotFoundException with ex.Exception_Message(e) & ": " & name;
    end getenv;


    function lc(s : ustr.Unbounded_String) return ustr.Unbounded_String is
        lowered_str : ustr.Unbounded_String;
    begin
        lowered_str := ustr.to_unbounded_string("");
        for i in 1..ustr.length(s) loop
            ustr.append(lowered_str, char.to_lower(ustr.element(s, i)));
        end loop;
        return lowered_str;
    end lc;


    function endsWith(s : ustr.Unbounded_String; suffixToMatch : ustr.Unbounded_String) return Boolean is
        endsWith : Boolean := false;
    begin
        if lc(ustr.tail(s, ustr.length(suffixToMatch))) = lc(suffixToMatch) then
            endsWith := true;
        end if;
        return endsWith;
    end endsWith;


    function indexOf(search_in : ustr.Unbounded_String; search_for : ustr.Unbounded_String; start_position : Positive) return Natural is
    begin
        return ustr.index(search_in, ustr.to_string(search_for), start_position);
    end indexOf;



    function isExecutable(file : dir.Directory_Entry_Type) return Boolean is
        extensionMatches : Boolean := false;

        procedure checkExtension(ext : ustr_vect.Cursor) is
        begin
            if endsWith(ustr.to_unbounded_string(dir.simple_name(file)), ustr_vect.element(ext)) then
                extensionMatches := true;
            end if;
        end checkExtension;
            
    begin
        --if ustr_vect.is_empty(exts) then
            --return os.is_executable_file(dir.full_name(file)); -- UNIX
        --end if;
        ustr_vect.iterate(exts, checkExtension'access);
        return extensionMatches;
    end isExecutable;


    procedure split(parts : out ustr_vect.Vector;  splittor : in ustr.Unbounded_String;  to_split : in ustr.Unbounded_String) is
        splittor_position : Natural;
        startFrom : Positive := Positive'first;
    begin
        ustr_vect.clear(parts);
        splittor_position := indexOf(to_split, splittor, startFrom);
        while splittor_position > 0 loop
            ustr_vect.append(parts, ustr.unbounded_slice(to_split, startFrom, splittor_position - 1));
            startFrom := splittor_position + ustr.length(splittor);
            splittor_position := indexOf(to_split, splittor, startFrom);
        end loop;
        if startFrom <= ustr.length(to_split) then
            ustr_vect.append(parts, ustr.unbounded_slice(to_split, startFrom, ustr.length(to_split)));
        end if;
    end split;


    function isDirectory(d : ustr.Unbounded_String) return Boolean is
        dir_name : String(1..ustr.length(d)) := ustr.to_string(d);
    begin
        return dir.exists(dir_name) and ( dir.file_kind'pos(dir.kind(dir_name)) = dir.file_kind'pos(dir.DIRECTORY) );
    end isDirectory;


    procedure which(searchTerm : String) is
        
        procedure searchInDirectory(directoriesCursor : ustr_vect.Cursor) is

            procedure checkFile(dirEntry : dir.Directory_Entry_Type) is
                matches : Boolean := false;
                searchTermPosition : Natural;
            begin
                if substring_search then
                    searchTermPosition := fstr.index(dir.simple_name(dirEntry), searchTerm, 1);
                    if searchTermPosition > 0 and isExecutable(dirEntry) then
                        tio.put_line(dir.full_name(dirEntry));
                    end if;
                elsif searchTerm = dir.simple_name(dirEntry) and isExecutable(dirEntry) then
                    tio.put_line(dir.full_name(dirEntry));
                end if;
            end checkFile;
        
        begin
            if verbose then
                uio.put_line("Checking dir: " & ustr_vect.element(directoriesCursor));
            end if;

            if isDirectory(ustr_vect.element(directoriesCursor)) then
                dir.search(to_string(ustr_vect.element(directoriesCursor)), "", (others => true), checkFile'access);
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
    e : ex.Exception_Occurrence;
begin
    if env.exists("PATHEXT") then
        path_sep := to_unbounded_string(";");
        split(exts, path_sep, getenv("PATHEXT"));
    else
        path_sep := to_unbounded_string(":");
    end if;

    split(path_dirs, path_sep, getenv("PATH"));

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

